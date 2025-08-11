use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use gurtlang::ast::TypedASTNode;
use gurtlang::ir::IRBuilder;
use gurtlang::lexer::Token;
use gurtlang::parser::Parser;
use gurtlang::typechecker::TypeEnv;
use gurtlang::{codegen::LLVMCodegen, validation};
use inkwell::context::Context;
use inkwell::targets::{InitializationConfig, Target};
use logos::Logos;

use std::ops::Range;
use std::{fs, process::Command};

pub struct CompilationPipeline {
    filepath: String,
    output_dir: String,
    optimize: bool,
    emit_llvm_ir: bool,
}

impl CompilationPipeline {
    pub fn new(filepath: &str) -> Self {
        Self {
            filepath: filepath.to_string(),
            output_dir: "target".to_string(),
            optimize: true,
            emit_llvm_ir: true,
        }
    }

    pub fn with_optimization(mut self, optimize: bool) -> Self {
        self.optimize = optimize;
        self
    }

    pub fn emit_ir(mut self, emit: bool) -> Self {
        self.emit_llvm_ir = emit;
        self
    }

    pub fn compile(&self) -> Result<String, CompilationError> {
        // Ensure output directory exists
        fs::create_dir_all(&self.output_dir)?;

        println!("🚀 Compiling: {}", self.filepath);

        // Step 1: Read source file
        let contents = fs::read_to_string(&self.filepath)
            .map_err(|e| CompilationError::IoError(format!("Failed to read file: {}", e)))?;

        // Step 2: Lexical analysis
        println!("📝 Lexical analysis...");
        self.lex_analysis(&contents)?;

        // Step 3: Parsing
        println!("🌳 Parsing...");
        let ast = self.parse(&contents)?;

        // Step 4: Type checking
        println!("🔍 Type checking...");
        let typed_ast = self.type_check(ast)?;

        // Step 5: IR generation
        println!("⚡ Generating IR...");
        let ir_module = self.generate_ir(typed_ast)?;

        // Step 6: LLVM codegen
        println!("🔧 Generating LLVM IR...");
        let llvm_output = self.generate_llvm_ir(ir_module)?;

        // Step 7: Compile to executable
        println!("🏗️  Compiling to executable...");
        let executable_path = self.compile_to_executable(&llvm_output)?;

        println!("✅ Compilation successful! Output: {}", executable_path);
        Ok(executable_path)
    }

    fn lex_analysis(&self, contents: &str) -> Result<(), CompilationError> {
        let lexer = Token::lexer(contents).spanned();
        let mut has_errors = false;

        for (token, span) in lexer {
            if token.is_err() {
                has_errors = true;
                self.report_lex_error(&span, contents);
            }
        }

        if has_errors {
            Err(CompilationError::LexError)
        } else {
            Ok(())
        }
    }

    fn parse(
        &self,
        contents: &str,
    ) -> Result<Vec<(gurtlang::ast::ASTNode, Range<usize>)>, CompilationError> {
        let lexer = Token::lexer(contents).spanned().peekable();
        let mut parser = Parser::new(lexer, self.filepath.clone());
        let ast = parser.parse_program();

        if parser.report_errors() {
            Err(CompilationError::ParseError)
        } else {
            Ok(ast)
        }
    }

    fn type_check(
        &self,
        ast: Vec<(gurtlang::ast::ASTNode, Range<usize>)>,
    ) -> Result<Vec<TypedASTNode>, CompilationError> {
        let mut type_env = TypeEnv::new(self.filepath.clone());
        type_env.add_builtins();

        // Add standard library types and functions
        self.add_standard_library(&mut type_env);

        let typed_ast = type_env.ast_to_typed_ast(ast);

        if type_env.report_errors() {
            return Err(CompilationError::TypeError);
        }

        let resolved_ast = type_env.resolve_all(typed_ast);
        let resolved_ast = resolved_ast
            .into_iter()
            .map(|node| match node {
                TypedASTNode::Expr((expr, span)) => {
                    TypedASTNode::Expr((type_env.builtin_macro_evaluation(expr), span))
                }
                TypedASTNode::Function((mut func, span)) => {
                    func.body =
                        Box::new((type_env.builtin_macro_evaluation(func.body.0), func.body.1));
                    TypedASTNode::Function((func, span))
                }
                _ => node,
            })
            .collect();

        let mono_ast = type_env.monomorphize_ast(resolved_ast);

        // Validation
        let validation_errors = validation::validate_ast(&mono_ast, self.filepath.clone());
        if !validation_errors.is_empty() {
            for (_, span, error) in validation_errors {
                self.report_validation_error(&span);
                println!("validation error: {error} at {span:?}")
            }
            return Err(CompilationError::ValidationError);
        }

        Ok(mono_ast)
    }

    fn generate_ir(
        &self,
        typed_ast: Vec<TypedASTNode>,
    ) -> Result<gurtlang::ir::Module, CompilationError> {
        let mut ir_builder = IRBuilder::new(self.filepath.clone());
        let ir_module = ir_builder.generate_module(typed_ast);

        if self.emit_llvm_ir {
            println!("Generated IR module: {}", ir_module.name);
            println!("{}", ir_module);
        }

        Ok(ir_module)
    }

    fn generate_llvm_ir(
        &self,
        ir_module: gurtlang::ir::Module,
    ) -> Result<String, CompilationError> {
        Target::initialize_all(&InitializationConfig::default());

        let context = Context::create();
        let mut codegen = LLVMCodegen::new(&context, "gurt_program");

        // Generate LLVM IR
        codegen.generate_module(&ir_module);

        // Verify the module before proceeding
        if let Err(err) = codegen.get_module().verify() {
            return Err(CompilationError::CodegenError(format!(
                "LLVM Module verification failed: {:?}",
                err
            )));
        }

        println!("LLVM Module verification passed!");

        // Emit to file
        let output_path = format!("{}/output.ll", self.output_dir);
        codegen
            .emit_to_file(&output_path)
            .map_err(CompilationError::CodegenError)?;

        Ok(output_path)
    }

    fn compile_to_executable(&self, llvm_ir_path: &str) -> Result<String, CompilationError> {
        let runtime_path = self.compile_runtime()?;
        let object_path = format!("{}/output.o", self.output_dir);
        let executable_path = format!("{}/executable", self.output_dir);
        let optimized_ir_path = format!("{}/optimized.ll", self.output_dir);

        // First, run optimization passes
        let output = Command::new("opt-18")
            .args([
                "-S",
                "-passes=default<O3>",
                llvm_ir_path,
                "-o",
                &optimized_ir_path,
            ])
            .output()
            .map_err(|e| CompilationError::LinkError(format!("Failed to run opt: {}", e)))?;

        if !output.status.success() {
            return Err(CompilationError::LinkError(format!(
                "Optimization failed: {}",
                String::from_utf8_lossy(&output.stderr)
            )));
        }

        // Compile to object with LTO support
        let output = Command::new("llc-18")
            .args([
                "-relocation-model=pic",
                "-O3",
                &optimized_ir_path,
                "-o",
                &object_path.replace(".o", ".s"),
            ])
            .output()
            .map_err(|e| CompilationError::LinkError(format!("Failed to run llc: {}", e)))?;

        if !output.status.success() {
            return Err(CompilationError::LinkError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        // Assemble to object file
        let output = Command::new("clang")
            .args([
                "-c",
                "-O3",
                "-fPIC",
                &object_path.replace(".o", ".s"),
                "-o",
                &object_path,
            ])
            .output()
            .map_err(|e| CompilationError::LinkError(format!("Failed to run clang: {}", e)))?;

        if !output.status.success() {
            return Err(CompilationError::LinkError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        // Link with Boehm GC
        let output = Command::new("clang")
            .args([
                "-flto",
                "-O3",
                "-no-pie",
                "-lm",
                "-lgc",
                &runtime_path,
                &object_path,
                "-o",
                &executable_path,
            ])
            .output()
            .map_err(|e| CompilationError::LinkError(format!("Failed to link: {}", e)))?;

        if !output.status.success() {
            return Err(CompilationError::LinkError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(executable_path)
    }

    fn compile_runtime(&self) -> Result<String, CompilationError> {
        // runtime.c exists in the runtime/ directory
        let runtime_c_path = "runtime/runtime.c";
        let runtime_o_path = format!("{}/runtime.o", self.output_dir);

        // Compile runtime with Boehm GC
        let output = Command::new("clang")
            .args([
                "-c",
                "-flto",
                "-O3",
                "-DUSE_BOEHM_GC", // Define macro for Boehm GC
                runtime_c_path,
                "-o",
                &runtime_o_path,
            ])
            .output()
            .map_err(|e| {
                CompilationError::LinkError(format!("Failed to compile runtime: {}", e))
            })?;

        if !output.status.success() {
            return Err(CompilationError::LinkError(
                String::from_utf8_lossy(&output.stderr).to_string(),
            ));
        }

        Ok(runtime_o_path)
    }

    pub fn benchmark(
        &self,
        executable_path: &str,
        iterations: u32,
    ) -> Result<(), CompilationError> {
        println!("Benchmarking executable: {}", executable_path);
        println!("Running {} iterations...", iterations);

        let mut total_time = std::time::Duration::new(0, 0);
        let mut times = Vec::new();

        for i in 1..=iterations {
            let start = std::time::Instant::now();

            let output = Command::new(executable_path)
                .output()
                .map_err(|e| CompilationError::Other(format!("Failed to run executable: {}", e)))?;

            let duration = start.elapsed();
            times.push(duration);
            total_time += duration;

            if !output.status.success() {
                return Err(CompilationError::Other(format!(
                    "Executable failed on iteration {}: {}",
                    i,
                    String::from_utf8_lossy(&output.stderr)
                )));
            }

            print!(".");
            std::io::Write::flush(&mut std::io::stdout()).unwrap();
        }

        println!();

        // Calculate statistics
        let avg_time = total_time / iterations;
        times.sort();
        let median_time = times[times.len() / 2];
        let min_time = times[0];
        let max_time = times[times.len() - 1];

        println!("📊 Benchmark Results:");
        println!("  Average: {:?}", avg_time);
        println!("  Median:  {:?}", median_time);
        println!("  Min:     {:?}", min_time);
        println!("  Max:     {:?}", max_time);
        println!("  Total:   {:?}", total_time);

        Ok(())
    }

    fn add_standard_library(&self, type_env: &mut TypeEnv) {
        gurtlang::stdlib::add_stdlib_to_env(type_env);
    }

    fn report_lex_error(&self, span: &Range<usize>, contents: &str) {
        let error: Report<'_, (String, Range<usize>)> =
            Report::build(ReportKind::Error, (self.filepath.clone(), span.clone()))
                .with_code("LEX001")
                .with_label(
                    Label::new((self.filepath.clone(), span.clone()))
                        .with_message("Invalid token")
                        .with_color(ColorGenerator::new().next()),
                )
                .with_message("Lexical analysis failed")
                .finish();

        let source = Source::from(contents);
        error.print((self.filepath.clone(), source)).unwrap();
    }

    fn report_validation_error(&self, _span: &Range<usize>) {}
}

#[derive(Debug)]
pub enum CompilationError {
    IoError(String),
    LexError,
    ParseError,
    TypeError,
    ValidationError,
    CodegenError(String),
    LinkError(String),
    Other(String),
}

impl From<std::io::Error> for CompilationError {
    fn from(error: std::io::Error) -> Self {
        CompilationError::IoError(error.to_string())
    }
}

impl std::fmt::Display for CompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            CompilationError::IoError(msg) => write!(f, "IO Error: {}", msg),
            CompilationError::LexError => write!(f, "Lexical analysis failed"),
            CompilationError::ParseError => write!(f, "Parsing failed"),
            CompilationError::TypeError => write!(f, "Type checking failed"),
            CompilationError::ValidationError => write!(f, "Validation failed"),
            CompilationError::CodegenError(msg) => write!(f, "Code generation error: {}", msg),
            CompilationError::LinkError(msg) => write!(f, "Linking error: {}", msg),
            CompilationError::Other(msg) => write!(f, "Something went wrong: {msg}"),
        }
    }
}

impl std::error::Error for CompilationError {}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let binding = "examples/3.gurt".to_string();
    let filepath = args.get(1).unwrap_or(&binding);

    match CompilationPipeline::new(filepath)
        .with_optimization(true)
        .emit_ir(false)
        .compile()
    {
        Ok(executable) => {
            println!("✅ Compilation successful!");
            println!("Executable: {}", executable);

            // Benchmark if requested
            if args.contains(&"--bench".to_string()) {
                let iterations = args
                    .iter()
                    .position(|arg| arg == "--iterations")
                    .and_then(|i| args.get(i + 1))
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(100);

                CompilationPipeline::new(filepath)
                    .benchmark(&executable, iterations)
                    .expect("Benchmarking failed");
            }

            // Run once if requested
            if args.contains(&"--run".to_string()) {
                println!("\n🏃 Running program:");
                std::process::Command::new(&executable)
                    .status()
                    .expect("Failed to run executable");
            }
        }
        Err(e) => {
            eprintln!("❌ Compilation failed: {}", e);
            std::process::exit(1);
        }
    }
}

#[cfg(test)]
#[test]
fn test_main() {
    match CompilationPipeline::new("examples/3.gurt").compile() {
        Ok(_) => println!("Test passed"),
        Err(e) => panic!("Test failed: {}", e),
    }
}

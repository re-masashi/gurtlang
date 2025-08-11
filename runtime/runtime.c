#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>
#include <ctype.h>
#include <stdbool.h>
#include <gc.h>  // Boehm GC header

// String object (simplified for Boehm GC)
typedef struct {
    size_t length;
    char data[];  // Flexible array member
} string_obj_t;

// Array object (simplified for Boehm GC)
typedef struct {
    size_t length;
    size_t capacity;
    size_t element_size;
    void* data[];  // Flexible array member
} array_obj_t;

#define TYPE_STRING 1
#define TYPE_ARRAY 2
#define TYPE_INT 3
#define TYPE_FLOAT 4
#define TYPE_BOOL 5

// Initialize Boehm GC (called automatically when library loads)
__attribute__((constructor))
void init_gc() {
    GC_INIT();
    printf("Boehm GC initialized\n");
}

// Basic GC operations (simplified - Boehm GC handles everything)
void* gc_alloc(size_t size, uint32_t type_id) {
    (void)type_id; // Unused with Boehm GC
    void* ptr = GC_MALLOC(size);
    if (!ptr) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return ptr;
}

// These are no-ops with Boehm GC, but kept for compatibility
void gc_retain(void* ptr) {
    (void)ptr; // No-op with Boehm GC
}

void gc_release(void* ptr) {
    (void)ptr; // No-op with Boehm GC
}

// String operations
void* string_concat(void* left, void* right) {
    if (!left || !right) return NULL;
    
    // Consistent string layout: [length][data...]
    size_t l_len = *(size_t*)left;
    char* l_data = (char*)left + sizeof(size_t);
    
    size_t r_len = *(size_t*)right;  
    char* r_data = (char*)right + sizeof(size_t);
    
    // Create result
    size_t new_len = l_len + r_len;
    void* result = gc_alloc(sizeof(size_t) + new_len + 1, TYPE_STRING);
    
    // Store length
    *(size_t*)result = new_len;
    
    // Store concatenated data
    char* result_data = (char*)result + sizeof(size_t);
    memcpy(result_data, l_data, l_len);
    memcpy(result_data + l_len, r_data, r_len);
    result_data[new_len] = '\0';
    
    return result;
}

int64_t string_length(void* str) {
    if (!str) return 0;
    size_t length = *(size_t*)str;
    return (int64_t)length;
}

// Array operations (simplified for integer arrays)
void* array_new(int64_t element_size, int64_t length) {
    if (length < 0) return NULL;
    
    size_t total_size = sizeof(array_obj_t) + (sizeof(int64_t) * length);
    array_obj_t* arr = (array_obj_t*)gc_alloc(total_size, TYPE_ARRAY);
    
    arr->length = length;
    arr->capacity = length;
    arr->element_size = element_size;
    
    // Initialize to zero
    memset(arr->data, 0, sizeof(int64_t) * length);
    
    return arr;
}

int64_t array_get(void* array_ptr, int64_t index) {
    if (!array_ptr) return 0;
    
    array_obj_t* arr = (array_obj_t*)array_ptr;
    if (index < 0 || index >= arr->length) return 0;
    
    // Treat data as int64_t array
    int64_t* data_ptr = (int64_t*)arr->data;
    return data_ptr[index];
}

void array_set(void* array_ptr, int64_t index, int64_t value) {
    if (!array_ptr) return;
    
    array_obj_t* arr = (array_obj_t*)array_ptr;
    if (index < 0 || index >= arr->length) return;
    
    // Store value directly
    int64_t* data_ptr = (int64_t*)arr->data;
    data_ptr[index] = value;
}

int64_t array_length(void* arr) {
    if (!arr) return 0;
    array_obj_t* a = (array_obj_t*)arr;
    return (int64_t)a->length;
}

void* array_concat(void* left, void* right, int64_t element_size) {
    if (!left || !right) return NULL;
    
    array_obj_t* l = (array_obj_t*)left;
    array_obj_t* r = (array_obj_t*)right;
    
    int64_t new_length = l->length + r->length;
    void* result = array_new(element_size, new_length);
    array_obj_t* res = (array_obj_t*)result;
    
    // Copy elements (treating as int64_t for simplicity)
    int64_t* l_data = (int64_t*)l->data;
    int64_t* r_data = (int64_t*)r->data;
    int64_t* res_data = (int64_t*)res->data;
    
    memcpy(res_data, l_data, l->length * sizeof(int64_t));
    memcpy(res_data + l->length, r_data, r->length * sizeof(int64_t));
    
    return result;
}

// Generic array operations (for object arrays)
void* gc_array_new(int32_t type_id, int64_t length) {
    (void)type_id; // Unused
    return array_new(sizeof(void*), length);
}

void* gc_array_get(void* arr, int64_t index) {
    if (!arr) return NULL;
    array_obj_t* a = (array_obj_t*)arr;
    if (index < 0 || index >= (int64_t)a->length) return NULL;
    return a->data[index];
}

void gc_array_set(void* arr, int64_t index, void* value) {
    if (!arr) return;
    array_obj_t* a = (array_obj_t*)arr;
    if (index < 0 || index >= (int64_t)a->capacity) return;
    
    // With Boehm GC, no need for retain/release
    a->data[index] = value;
    
    // Update length if necessary
    if (index >= (int64_t)a->length) {
        a->length = index + 1;
    }
}

// Type conversion functions
void* int_to_string(int64_t value) {
    char buffer[32];
    snprintf(buffer, sizeof(buffer), "%ld", (long)value);
    size_t len = strlen(buffer);
    
    void* result = gc_alloc(sizeof(size_t) + len + 1, TYPE_STRING);
    
    // Store length at offset 0
    *(size_t*)result = len;
    
    // Store data at offset sizeof(size_t)
    char* data_ptr = (char*)result + sizeof(size_t);
    memcpy(data_ptr, buffer, len);
    data_ptr[len] = '\0';
    
    return result;
}

void* float_to_string(double value) {
    char buffer[64];
    snprintf(buffer, sizeof(buffer), "%g", value);
    size_t len = strlen(buffer);
    
    void* result = gc_alloc(sizeof(size_t) + len + 1, TYPE_STRING);
    *(size_t*)result = len;
    
    char* data_ptr = (char*)result + sizeof(size_t);
    memcpy(data_ptr, buffer, len);
    data_ptr[len] = '\0';
    
    return result;
}

void* bool_to_string(uint8_t value) {
    const char* str = value ? "true" : "false";
    size_t len = strlen(str);
    
    void* result = gc_alloc(sizeof(size_t) + len + 1, TYPE_STRING);
    *(size_t*)result = len;
    
    char* data_ptr = (char*)result + sizeof(size_t);
    memcpy(data_ptr, str, len);
    data_ptr[len] = '\0';
    
    return result;
}

void* char_to_string(uint8_t value) {
    void* result = gc_alloc(sizeof(size_t) + 2, TYPE_STRING);
    *(size_t*)result = 1;
    
    char* data_ptr = (char*)result + sizeof(size_t);
    data_ptr[0] = (char)value;
    data_ptr[1] = '\0';
    
    return result;
}

// IO functions
void print_internal(void* str) {
    if (!str) {
        printf("null");
        return;
    }
    
    // Consistent string layout: [length][data...]
    size_t length = *(size_t*)str;
    char* data = (char*)str + sizeof(size_t);
    
    printf("%.*s", (int)length, data);
    fflush(stdout);
}

// Standard library functions
void* substring(void* str, int64_t start, int64_t end) {
    if (!str) return NULL;
    
    size_t str_len = *(size_t*)str;
    char* str_data = (char*)str + sizeof(size_t);
    
    if (start < 0) start = 0;
    if (end > (int64_t)str_len) end = str_len;
    if (start >= end) {
        // Return empty string
        void* result = gc_alloc(sizeof(size_t) + 1, TYPE_STRING);
        *(size_t*)result = 0;
        *((char*)result + sizeof(size_t)) = '\0';
        return result;
    }
    
    size_t len = end - start;
    void* result = gc_alloc(sizeof(size_t) + len + 1, TYPE_STRING);
    *(size_t*)result = len;
    
    char* data_ptr = (char*)result + sizeof(size_t);
    memcpy(data_ptr, str_data + start, len);
    data_ptr[len] = '\0';
    
    return result;
}

void* to_upper(void* str) {
    if (!str) return NULL;
    
    size_t str_len = *(size_t*)str;
    char* str_data = (char*)str + sizeof(size_t);
    
    void* result = gc_alloc(sizeof(size_t) + str_len + 1, TYPE_STRING);
    *(size_t*)result = str_len;
    
    char* result_data = (char*)result + sizeof(size_t);
    for (size_t i = 0; i < str_len; i++) {
        result_data[i] = toupper((unsigned char)str_data[i]);
    }
    result_data[str_len] = '\0';
    
    return result;
}

void* to_lower(void* str) {
    if (!str) return NULL;
    
    size_t str_len = *(size_t*)str;
    char* str_data = (char*)str + sizeof(size_t);
    
    void* result = gc_alloc(sizeof(size_t) + str_len + 1, TYPE_STRING);
    *(size_t*)result = str_len;
    
    char* result_data = (char*)result + sizeof(size_t);
    for (size_t i = 0; i < str_len; i++) {
        result_data[i] = tolower((unsigned char)str_data[i]);
    }
    result_data[str_len] = '\0';
    
    return result;
}

void* trim(void* str) {
    if (!str) return NULL;
    
    size_t str_len = *(size_t*)str;
    char* str_data = (char*)str + sizeof(size_t);
    
    size_t start = 0, end = str_len;
    
    // Find first non-whitespace
    while (start < str_len && isspace((unsigned char)str_data[start])) start++;
    
    // Find last non-whitespace
    while (end > start && isspace((unsigned char)str_data[end - 1])) end--;
    
    size_t result_len = end - start;
    void* result = gc_alloc(sizeof(size_t) + result_len + 1, TYPE_STRING);
    *(size_t*)result = result_len;
    
    char* result_data = (char*)result + sizeof(size_t);
    memcpy(result_data, str_data + start, result_len);
    result_data[result_len] = '\0';
    
    return result;
}

// Array functions
void* append(void* arr, void* element) {
    if (!arr) return NULL;
    array_obj_t* a = (array_obj_t*)arr;
    
    // Create new array with increased capacity
    void* new_arr = gc_array_new(TYPE_ARRAY, a->length + 1);
    array_obj_t* new_a = (array_obj_t*)new_arr;
    
    // Copy existing elements
    for (int64_t i = 0; i < (int64_t)a->length; i++) {
        gc_array_set(new_arr, i, a->data[i]);
    }
    
    // Add new element
    gc_array_set(new_arr, a->length, element);
    
    return new_arr;
}

void* reverse(void* arr) {
    if (!arr) return NULL;
    array_obj_t* a = (array_obj_t*)arr;
    
    void* new_arr = gc_array_new(TYPE_ARRAY, a->length);
    
    for (int64_t i = 0; i < (int64_t)a->length; i++) {
        gc_array_set(new_arr, i, a->data[a->length - 1 - i]);
    }
    
    return new_arr;
}

// Math functions
int64_t abs_int(int64_t x) {
    return x < 0 ? -x : x;
}

double sqrt_float(double x) {
    return sqrt(x);
}

int64_t max_int(int64_t a, int64_t b) {
    return a > b ? a : b;
}

int64_t min_int(int64_t a, int64_t b) {
    return a < b ? a : b;
}

double pow_float(double base, double exp) {
    return pow(base, exp);
}

int64_t floor_float(double x) {
    return (int64_t)floor(x);
}

int64_t ceil_float(double x) {
    return (int64_t)ceil(x);
}

// GC stats (for debugging)
void gc_stats() {
    printf("GC heap size: %lu bytes\n", GC_get_heap_size());
    printf("GC free bytes: %lu bytes\n", GC_get_free_bytes());
}

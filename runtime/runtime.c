#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdint.h>   // For uint32_t, uint8_t, etc.
#include <ctype.h>    // For toupper, isspace, etc.
#include <stdbool.h>

// GC header for all managed objects
typedef struct {
    uint32_t ref_count;
    uint32_t type_id;
    size_t size;
} gc_header_t;

// String object
typedef struct {
    gc_header_t header;
    size_t length;
    char data[];
} string_obj_t;

// Array object
typedef struct {
    gc_header_t header;
    size_t length;
    size_t capacity;
    void* data[];
} array_obj_t;

#define TYPE_STRING 1
#define TYPE_ARRAY 2
#define TYPE_INT 3
#define TYPE_FLOAT 4
#define TYPE_BOOL 5

// Basic GC operations
void* gc_alloc(size_t size, uint32_t type_id) {
    gc_header_t* obj = malloc(sizeof(gc_header_t) + size);
    if (!obj) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    obj->ref_count = 1;
    obj->type_id = type_id;
    obj->size = size;
    return (char*)obj + sizeof(gc_header_t);
}

void gc_retain(void* ptr) {
    if (!ptr) return;
    gc_header_t* header = (gc_header_t*)((char*)ptr - sizeof(gc_header_t));
    header->ref_count++;
}

void gc_release(void* ptr) {
    if (!ptr) return;

    // printf("DEBUG: gc_release called with ptr=%p\n", ptr);
    
    // Validate pointer is not obviously corrupted
    if ((uintptr_t)ptr < 0x1000) {
        fprintf(stderr, "Error: Attempting to release invalid pointer %p\n", ptr);
        return;
    }
    
    gc_header_t* header = (gc_header_t*)((char*)ptr - sizeof(gc_header_t));
    
    // Sanity check the header
    if (header->ref_count == 0) {
        fprintf(stderr, "Error: Double-free detected on pointer %p\n", ptr);
        return;
    }
    
    if (header->ref_count > 10000) {
        fprintf(stderr, "Error: Corrupted ref_count %u on pointer %p\n", header->ref_count, ptr);
        return;
    }
    
    if (--header->ref_count == 0) {
        free(header);
    }
}


// String operations
void* string_concat(void* left, void* right) {
    // fprintf(stderr, "DEBUG: string_concat with left=%p, right=%p\n", left, right);
    
    if (!left || !right) return NULL;
    
    // All strings are now managed - no detection needed!
    size_t l_len = *(size_t*)left;
    char* l_data = (char*)left + sizeof(size_t);
    
    size_t r_len = *(size_t*)right;  
    char* r_data = (char*)right + sizeof(size_t);
    
    // fprintf(stderr, "DEBUG: Left len=%zu data='%.*s'\n", l_len, (int)l_len, l_data);
    // fprintf(stderr, "DEBUG: Right len=%zu data='%.*s'\n", r_len, (int)r_len, r_data);
    
    // Create result with consistent layout
    size_t new_len = l_len + r_len;
    void* result = gc_alloc(sizeof(size_t) + new_len + 1, TYPE_STRING);
    
    // Store length
    *(size_t*)result = new_len;
    
    // Store concatenated data
    char* result_data = (char*)result + sizeof(size_t);
    memcpy(result_data, l_data, l_len);
    memcpy(result_data + l_len, r_data, r_len);
    result_data[new_len] = '\0';
    
    // fprintf(stderr, "DEBUG: Result len=%zu data='%s'\n", new_len, result_data);
    return result;
}

int64_t string_length(void* str) {
    if (!str) return 0;
    size_t length = *(size_t*)str;
    return (int64_t)length;
}


// Array operations
void* gc_array_new(int32_t type_id, int64_t length) {
    size_t capacity = length > 0 ? length : 4;
    array_obj_t* obj = (array_obj_t*)gc_alloc(sizeof(array_obj_t) + capacity * sizeof(void*), TYPE_ARRAY);
    obj->length = length;
    obj->capacity = capacity;
    
    // Initialize to null
    for (size_t i = 0; i < capacity; i++) {
        obj->data[i] = NULL;
    }
    
    return obj;
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
    
    // Check if old value is valid before releasing
    void* old_value = a->data[index];
    if (old_value) {
        // Validate the old value has a proper GC header
        gc_header_t* old_header = (gc_header_t*)((char*)old_value - sizeof(gc_header_t));
        
        // Basic sanity check - ref_count should be reasonable
        if (old_header->ref_count > 0 && old_header->ref_count < 10000) {
            gc_release(old_value);
        } else {
            // Don't release - this pointer is probably corrupted
            fprintf(stderr, "Warning: Skipping release of potentially corrupted pointer %p\n", old_value);
        }
    }
    
    // Retain new value
    if (value) {
        gc_retain(value);
    }
    
    a->data[index] = value;
    
    // Update length if necessary
    if (index >= (int64_t)a->length) {
        a->length = index + 1;
    }
}

int64_t array_length(void* arr) {
    if (!arr) return 0;
    array_obj_t* a = (array_obj_t*)arr;
    return (int64_t)a->length;
}

void* array_concat(void* left, void* right, int32_t element_type) {
    if (!left || !right) return NULL;
    array_obj_t* l = (array_obj_t*)left;
    array_obj_t* r = (array_obj_t*)right;
    
    int64_t new_length = l->length + r->length;
    void* result = gc_array_new(element_type, new_length);
    array_obj_t* res = (array_obj_t*)result;
    
    // Copy elements from left array
    for (int64_t i = 0; i < (int64_t)l->length; i++) {
        gc_array_set(result, i, l->data[i]);
    }
    
    // Copy elements from right array
    for (int64_t i = 0; i < (int64_t)r->length; i++) {
        gc_array_set(result, (int64_t)l->length + i, r->data[i]);
    }
    
    return result;
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
    
    // All strings have the same layout now
    size_t length = *(size_t*)str;
    char* data = (char*)str + sizeof(size_t);
    
    printf("%s", data);
    fflush(stdout);
}

// Standard library functions
void* substring(void* str, int64_t start, int64_t end) {
    if (!str) return NULL;
    
    // Use new layout
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
    string_obj_t* s = (string_obj_t*)str;
    
    string_obj_t* result = (string_obj_t*)gc_alloc(sizeof(string_obj_t) + s->length + 1, TYPE_STRING);
    result->length = s->length;
    
    for (size_t i = 0; i < s->length; i++) {
        result->data[i] = tolower((unsigned char)s->data[i]);
    }
    result->data[s->length] = '\0';
    
    return result;
}

void* trim(void* str) {
    if (!str) return NULL;
    
    // Use new managed layout: [length][data]
    size_t str_len = *(size_t*)str;
    char* str_data = (char*)str + sizeof(size_t);
    
    size_t start = 0, end = str_len;
    
    // Find first non-whitespace
    while (start < str_len && isspace((unsigned char)str_data[start])) start++;
    
    // Find last non-whitespace
    while (end > start && isspace((unsigned char)str_data[end - 1])) end--;
    
    // Create result with new layout
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

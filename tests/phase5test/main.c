#include <stdio.h>         /* make sure these two includes are    */
#include <inttypes.h>      /* present in the start of your C file */

// your LLVM program must produce a function called dolphin_main of the following type.
extern int64_t dolphin_main();

int64_t read_integer () {
    int64_t value;
    printf("Please enter an integer: ");
    scanf("%" PRId64 "" , &value);
    return value;
}

void print_integer (int64_t value) {
    printf("%" PRId64 "\n" , value);
}

int main(){
    return dolphin_main();
}

#include <stdint.h>
#include <string.h>

// Define the array structure matching the %array_type in LLVM IR
struct array {
    int64_t len;         // Length of the string
    char contents[];     // Contents of the string
};

// The compare_strings function
int64_t compare_strings(struct array *str1, struct array *str2) {
    if (str1 == NULL || str2 == NULL) {
        // Handle NULL pointers: NULL is considered less than any valid pointer
        return (str1 == str2) ? 0 : (str1 == NULL ? -1 : 1);
    }

    // Compare string contents up to the length of the shorter string
    int cmp = strncmp(str1->contents, str2->contents, (size_t)(str1->len < str2->len ? str1->len : str2->len));
    if (cmp != 0) {
        // If contents differ, return the result of comparison
        return cmp < 0 ? -1 : 1;
    }

    // If contents are equal, compare lengths
    if (str1->len < str2->len) return -1;
    if (str1->len > str2->len) return 1;

    // Strings are identical
    return 0;
}
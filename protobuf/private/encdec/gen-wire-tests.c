#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

void gen_int_test(const char* reader,
                  void* ptr,
                  size_t bits,
                  int64_t expected)
{
    printf("(check-eqv? (%s (open-input-bytes (bytes", reader);
    for (size_t i = 0; i < (bits / 8); i++) {
        printf(" #x%x", ((unsigned char*) ptr)[i]);
    }
    printf("))) %lld)\n", expected);
}

void gen_flo_test(const char* reader,
                  void* ptr,
                  size_t bits,
                  double expected)
{
    printf("(check-= (%s (open-input-bytes (bytes", reader);
    for (size_t i = 0; i < (bits / 8); i++) {
        printf(" %d", ((unsigned char*) ptr)[i]);
    }
    printf("))) %.3f 0.01)\n", expected);
}


int64_t rand_signed64()
{
    char bytes[8];
    for (int i = 0; i < 8; i++)
        bytes[i] = rand();

    return *(int64_t*) bytes;
}

int main()
{
    srand(time(NULL));
    for (int i = 0; i < 4; i++) {
        int64_t i64 = rand_signed64();
        uint64_t u64 = (uint64_t) i64;
        if (i64 < 0) u64 = -i64;
        int32_t i32 = (int32_t) i64;
        uint32_t u32 = (uint32_t) u64;
        if (i32 < 0) u32 = -i32;

        gen_int_test("read-sfixed64", &i64, 64, i64);
        gen_int_test("read-fixed64 ", &u64, 64, u64);
        gen_int_test("read-sfixed32", &i32, 32, i32);
        gen_int_test("read-fixed32 ", &u32, 32, u32);

        double dbl = rand_signed64() / (double) rand_signed64() * 50000;
        float flt = dbl;

        gen_flo_test("read-double", &dbl, 64, dbl);
        gen_flo_test("read-float ", &flt, 32, flt);
    }
}

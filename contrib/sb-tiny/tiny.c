/* A tiny C library for exercising SBCL's static executable build flow.
   It provides the three foreign symbols declared by sb-tiny.lisp in
   this directory.  See README.static-executable and test-static.sh.

   Build a static library from this file with:
     cc -O2 -c tiny.c && ar rcs libsb-tiny.a tiny.o
 */

int tiny_add(int a, int b)
{
    return a + b;
}

int tiny_mul(int a, int b)
{
    return a * b;
}

/* The leading declaration is what gives this definition external
   linkage (plain top-level const objects have internal linkage in C),
   so that the system linker can resolve references to it when
   statically linking the SBCL runtime. */
extern const int tiny_answer;
const int tiny_answer = 42;

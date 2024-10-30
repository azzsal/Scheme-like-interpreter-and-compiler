#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

extern uint64_t entry(void *heap);

#define num_tag   0b00
#define num_mask  0b11
#define num_shift 2

#define bool_tag   0b0011111
#define bool_mask  0b1111111
#define bool_shift 7

#define pair_tag   0b010
#define pair_mask  0b111

#define heap_size  4096

void print_value(uint64_t value) {
  if ((value & num_mask) == num_tag) {
    int64_t ivalue = (int64_t) value;
    printf("%" PRIi64, (ivalue >> num_shift));
  } else if ((value & bool_mask) == bool_tag) {
    if(value >> bool_shift) {
      printf("true");
    } else {
      printf("false");
    }
  } else if ((value & pair_mask) == pair_tag) {
      uint64_t v1 = *(uint64_t *)(value - pair_tag);
      uint64_t v2 = *(uint64_t *)(value - pair_tag + 8);
      printf("(pair ");
      print_value(v1);
      printf(" ");
      print_value(v2);
      printf(")");
  } else {
    printf("BAD VALUE %" PRIu64, value);
  }
}

void error() {
  printf("ERROR");
  exit(1);
}

uint64_t read_num() {
  int r;
  scanf("%d", &r);
  return (uint64_t)(r) << num_shift;
}

void print_newline() {
  printf("\n");
}

int main(int argc, char **argv) {
  void* heap = (void *) malloc(heap_size);
  entry(heap);
  return 0;
}
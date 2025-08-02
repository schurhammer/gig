
#include <stdlib.h>
#include <string.h>

#define INITIAL_ARENA_SIZE 8 * 1024 * 1024 // 8 MB
#define ALIGNMENT 8

typedef struct Arena {
  size_t size;
  size_t used;
  struct Arena *next;
  char data[];
} Arena;

static Arena *current_arena = NULL;

static size_t align_up(size_t size, size_t alignment) {
  return (size + alignment - 1) & ~(alignment - 1);
}

void *arena_malloc(size_t size) {
  size = align_up(size, ALIGNMENT);

  if (!current_arena || current_arena->used + size > current_arena->size) {
    size_t arena_size =
        INITIAL_ARENA_SIZE > size ? INITIAL_ARENA_SIZE : size + sizeof(Arena);
    Arena *new_arena = (Arena *)malloc(sizeof(Arena) + arena_size);

    if (!new_arena) {
      return NULL; // Allocation failed
    }

    new_arena->size = arena_size;
    new_arena->used = 0;
    new_arena->next = current_arena;
    current_arena = new_arena;
  }

  void *ptr = current_arena->data + current_arena->used;
  current_arena->used += size;

  return ptr;
}

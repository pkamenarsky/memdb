#include <map>
#include <string>
#include <cstdint>
#include <iostream>

extern "C" {
  std::multimap<std::string, int32_t> *new_mmap_str() {
    return new std::multimap<std::string, int32_t>();
  }

  void delete_mmap_str(std::multimap<std::string, int32_t> *mm) {
    delete mm;
  }

  void insert_mmap_str(std::multimap<std::string, int32_t> *mm, char *k, int32_t v) {
    mm->insert(std::pair<std::string, int32_t>(k, v));
  }

  int32_t *lookup_mmap_str(std::multimap<std::string, int32_t> *mm, char *k) {
    std::pair <std::multimap<std::string, int32_t>::iterator, std::multimap<std::string, int32_t>::iterator> range
      = mm->equal_range(k);

    int32_t length = std::distance(range.first, range.second) + 1;
    int32_t *result = new int32_t[length];
    int32_t i = 0;

    result[0] = length;

    for (std::multimap<std::string, int32_t>::iterator it = range.first; it != range.second; ++it, ++i) {
      result[i + 1] = it->second;
    }

    return result;
  }

  std::multimap<int32_t, int32_t> *new_mmap_int() {
    return new std::multimap<int32_t, int32_t>();
  }

  void delete_mmap_int(std::multimap<int32_t, int32_t> *mm) {
    delete mm;
  }

  void insert_mmap_int(std::multimap<int32_t, int32_t> *mm, int32_t k, int32_t v) {
    mm->insert(std::pair<int32_t, int32_t>(k, v));
  }

  int32_t *lookup_mmap_int(std::multimap<int32_t, int32_t> *mm, int32_t k) {
    std::pair <std::multimap<int32_t, int32_t>::iterator, std::multimap<int32_t, int32_t>::iterator> range
      = mm->equal_range(k);

    int32_t length = std::distance(range.first, range.second) + 1;
    int32_t *result = new int32_t[length];
    int32_t i = 0;

    result[0] = length;

    for (std::multimap<int32_t, int32_t>::iterator it = range.first; it != range.second; ++it, ++i) {
      result[i + 1] = it->second;
    }

    return result;
  }

  void free_result(int32_t *result) {
    delete result;
  }
}

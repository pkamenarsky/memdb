#include <cstdint>
#include <iostream>
#include <map>
#include <string>

extern "C" {
  std::multimap<std::string, uint32_t> *new_mmap_str() {
    return new std::multimap<std::string, uint32_t>();
  }

  void delete_mmap_str(std::multimap<std::string, uint32_t> *mm) {
    delete mm;
  }

  void insert_mmap_str(std::multimap<std::string, uint32_t> *mm, char *k, uint32_t k_length, uint32_t v) {
    mm->insert(std::pair<std::string, uint32_t>(std::string(k, k_length), v));
  }

  uint32_t *lookup_mmap_str(std::multimap<std::string, uint32_t> *mm, char *k, uint32_t k_length) {
    std::pair <std::multimap<std::string, uint32_t>::iterator, std::multimap<std::string, uint32_t>::iterator> range
      = mm->equal_range(std::string(k, k_length));

    uint32_t length = std::distance(range.first, range.second) + 1;
    uint32_t *result = new uint32_t[length];
    uint32_t i = 0;

    result[0] = length;

    for (std::multimap<std::string, uint32_t>::iterator it = range.first; it != range.second; ++it, ++i) {
      result[i + 1] = it->second;
    }

    return result;
  }

  std::multimap<uint32_t, uint32_t> *new_mmap_int() {
    return new std::multimap<uint32_t, uint32_t>();
  }

  void delete_mmap_int(std::multimap<uint32_t, uint32_t> *mm) {
    delete mm;
  }

  void insert_mmap_int(std::multimap<uint32_t, uint32_t> *mm, uint32_t k, uint32_t v) {
    mm->insert(std::pair<uint32_t, uint32_t>(k, v));
  }

  uint32_t *lookup_mmap_int(std::multimap<uint32_t, uint32_t> *mm, uint32_t k) {
    std::pair <std::multimap<uint32_t, uint32_t>::iterator, std::multimap<uint32_t, uint32_t>::iterator> range
      = mm->equal_range(k);

    uint32_t length = std::distance(range.first, range.second) + 1;
    uint32_t *result = new uint32_t[length];
    uint32_t i = 0;

    result[0] = length;

    for (std::multimap<uint32_t, uint32_t>::iterator it = range.first; it != range.second; ++it, ++i) {
      result[i + 1] = it->second;
    }

    return result;
  }

  void free_result(uint32_t *result) {
    delete result;
  }
}

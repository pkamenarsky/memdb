#include <map>
#include <string>
#include <cstdint>
#include <iostream>

extern "C" {
  std::multimap<std::string, int32_t> *new_mmap() {
    return new std::multimap<std::string, int32_t>();
  }

  void delete_mmap(std::multimap<std::string, int32_t> *mm) {
    delete mm;
  }

  void insert_mmap(std::multimap<std::string, int32_t> *mm, char *k, int32_t v) {
    mm->insert(std::pair<std::string, int32_t>(k, v));
  }

  int32_t *lookup_mmap(std::multimap<std::string, int32_t> *mm, char *k) {
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

  void free_result(int32_t *result) {
    delete result;
  }
}

/*
int main() {
  std::multimap<std::string, int64_t> mm = std::multimap<std::string, int64_t>();
  mm.insert(std::pair<std::string, int64_t>("asd", 5));
  mm.insert(std::pair<std::string, int64_t>("asd", 6));

  std::pair <std::multimap<std::string, int64_t>::iterator, std::multimap<std::string, int64_t>::iterator> ret;
  ret = mm.equal_range("asd");
  for (std::multimap<std::string, int64_t>::iterator it=ret.first; it!=ret.second; ++it)
      std::cout << ' ' << it->second;
  return 0;
}
*/

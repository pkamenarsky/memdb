#include <map>
#include <string>
#include <cstdint>
#include <iostream>

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

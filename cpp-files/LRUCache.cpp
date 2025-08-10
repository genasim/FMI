#include <unordered_map>
#include <list>
#include <optional>

template<class Node, class Value>
class LRUCache {
    using Cache = std::list<std::pair<Node, Value>>;
    
public:
    explicit LRUCache(size_t capacity);
    
    std::optional<Value> get(const Node& key) noexcept;
    void put(const Node& key, const Value& value) noexcept;
    
private:
    Cache cache;
    std::unordered_map<Node, typename Cache::iterator> cacheMap;
    size_t capacity;
};



template<class Node, class Value>
LRUCache<Node, Value>::LRUCache(size_t capacity) : capacity(capacity) {}

template<class Node, class Value>
std::optional<Value> LRUCache<Node, Value>::get(const Node& key) noexcept {
    auto it = cacheMap.find(key);
    if (it == cacheMap.end()) {
        return std::nullopt;
    }
    
    typename Cache::iterator dataIt = it->second;
    cache.splice(cache.cbegin(), cache, dataIt);
    return std::make_optional(dataIt->second);
}

template<class Node, class Value>
void LRUCache<Node, Value>::put(const Node& key, const Value& value) noexcept {
    auto it = cacheMap.find(key);
    if (it != cacheMap.end()) {
        cache.erase(it->second);
    }
    
    cache.push_front({ key, value });
    cacheMap[key] = cache.begin();
    
    if (cache.size() > capacity) {
        cacheMap.erase(cache.back().first);
        cache.pop_back();
    }
}


#include <iostream>
#include <thread>
#include <chrono>
#include <string>

using namespace std::chrono_literals;

int long_process(int arg) {
    std::this_thread::sleep_for(3000ms);
    return arg * 2;
}

//int poll_cache(LRUCache<int, int>& cache, int arg) {
//    return cache.get(arg).or_else([&]() {     <-- C++23
//        int result = long_process(arg);
//        cache.put(arg, result);
//        return std::make_optional(result);
//    }).value();
//}


int poll_cache(LRUCache<int, int>& cache, int arg) {
    std::optional<int> cacheRes = cache.get(arg);
    if (cacheRes.has_value())
        return cacheRes.value();
        
    int res = long_process(arg);
    cache.put(arg, res);
    return res;
}


int main() {
    LRUCache<int, int> cache(2);
    int res;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 20);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 20);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 8);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 10);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 8);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 20);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 8);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 10);
    std::cout << res << std::endl;
    
    std::cout << "starting ... ";
    res = poll_cache(cache, 8);
    std::cout << res << std::endl;
    
    return 0;
}

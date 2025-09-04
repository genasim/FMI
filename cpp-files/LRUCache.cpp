#include <unordered_map>
#include <list>
#include <optional>

template<class Key, class Value>
class LRUCache {
    using Cache = std::list<std::pair<Key, Value>>;
    
public:
    explicit LRUCache(size_t capacity);
    
    std::optional<Value> get(const Key& key) noexcept;
    void put(const Key& key, const Value& value) noexcept;
    
private:
    Cache cache;
    std::unordered_map<Key, typename Cache::iterator> cacheMap;
    size_t capacity;
};



template<class Key, class Value>
LRUCache<Key, Value>::LRUCache(size_t capacity) : capacity(capacity) {}

template<class Key, class Value>
std::optional<Value> LRUCache<Key, Value>::get(const Key& key) noexcept {
    auto it = cacheMap.find(key);
    if (it == cacheMap.end()) {
        return std::nullopt;
    }
    
    typename Cache::iterator dataIt = it->second;
    cache.splice(cache.cbegin(), cache, dataIt);
    return std::make_optional(dataIt->second);
}

template<class Key, class Value>
void LRUCache<Key, Value>::put(const Key& key, const Value& value) noexcept {
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

#pragma once

template <class T>
class UniquePtr {
   private:
    T* ptr;
    void _free();

   public:
    explicit UniquePtr(T* rawPtr = nullptr);
    UniquePtr(UniquePtr&& other) noexcept;
    UniquePtr& operator=(UniquePtr&& other) noexcept;
    UniquePtr& operator=(T*&& other) noexcept;

    UniquePtr(const UniquePtr&) = delete;
    UniquePtr& operator=(const UniquePtr&) = delete;

    ~UniquePtr();

    T& operator*() const;
    T* operator->() const noexcept;

    bool isValid() const noexcept;
    T* release() noexcept;
    void reset(T* rawPtr = nullptr) noexcept;
};

#include "../../impl/utils/UniquePtr.tpp"
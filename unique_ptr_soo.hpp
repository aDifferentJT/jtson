#ifndef unique_ptr_soo_hpp
#define unique_ptr_soo_hpp

template <std::size_t size, typename T>
class unique_ptr_soo {
  private:
    std::array<uint8_t, size> small;
    T* data;
    auto (*copyConstructor)(uint8_t*, T const *) -> T*;
    auto (*moveConstructor)(uint8_t*, T*) -> T*;
    void (*destructor)(T*);

    unique_ptr_soo() = default;
  public:
    template <typename U>
    unique_ptr_soo(U x) {
      if constexpr (sizeof(U) <= size) {
        data = new(small.data()) U{std::move(x)};
        copyConstructor = [](uint8_t* small, T const * that) {
          return static_cast<T*>(new(small) U{*static_cast<U const *>(that)});
        };
        moveConstructor = [](uint8_t* small, T* that) {
          return static_cast<T*>(new(small) U{std::move(*static_cast<U*>(that))});
        };
        destructor = [](T* y) { static_cast<U*>(y)->~U(); };
      } else {
        data = new U{std::move(x)};
        copyConstructor = [](uint8_t*, T const * that) {
          return static_cast<T*>(new U{*static_cast<U const *>(that)});
        };
        moveConstructor = [](uint8_t*, T* that) {
          return static_cast<T*>(new U{std::move(*static_cast<U*>(that))});
        };
        destructor = [](T* y) { delete static_cast<U*>(y); };
      }
    }

    unique_ptr_soo(unique_ptr_soo const & that)
      : data{that.copyConstructor(small.data(), that.get())}
      , copyConstructor{that.copyConstructor}
      , moveConstructor{that.moveConstructor}
      , destructor{that.destructor}
      {}

    unique_ptr_soo& operator=(unique_ptr_soo const & that) {
      if (this != &that) {
        this->destructor(this->data);
        this->data = that.copyConstructor(this->small.data(), that.data);
        this->copyConstructor = that.copyConstructor;
        this->moveConstructor = that.moveConstructor;
        this->destructor = that.destructor;
      }
      return *this;
    }

    unique_ptr_soo(unique_ptr_soo&& that)
      : data{that.moveConstructor(small.data(), that.get())}
      , copyConstructor{that.copyConstructor}
      , moveConstructor{that.moveConstructor}
      , destructor{that.destructor}
      {}

    unique_ptr_soo& operator=(unique_ptr_soo&& that) {
      if (this != &that) {
        this->destructor(this->data);
        this->data = that.moveConstructor(this->small.data(), that.data);
        this->copyConstructor = that.copyConstructor;
        this->moveConstructor = that.moveConstructor;
        this->destructor = that.destructor;
      }
      return *this;
    }

    ~unique_ptr_soo() {
      destructor(data);
    }

    auto get()       -> T       * { return data; }
    auto get() const -> T const * { return data; }
};

#endif

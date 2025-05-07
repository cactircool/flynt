#ifndef SLIDINGWINDOW_H
#define SLIDINGWINDOW_H

#include <sys/types.h>
#include <cassert>
#include "flynt/preprocessor.h"

namespace flynt::util {

/**
 * @brief The SlidingWindow class: very cool, I just thought of it. Do I need it? No I don't think so. Was it fun? Yes, fuck off!
 * stack allocated btw
 */
template <typename T, size_t MaxSize = 0>
class SlidingWindow
{
public:
    /**
     * @brief SlidingWindow: create a sliding window with a specific static size (for performance and space)
     */
    SlidingWindow() : index(0) {
        FLYNT_ASSERT(MaxSize > 0);
    }

    SlidingWindow(const SlidingWindow &) = default;
    SlidingWindow(SlidingWindow &&) = default;

    ~SlidingWindow() = default;

    /**
     * @brief push: push an item to the sliding window, if the window is full, the first element is overwritten
     * @param item
     */
    void push(T item) {
        data[index] = item;
        index = (index + 1) % MaxSize;
    }
    /**
     * @brief pop: remove the last element of the window, popping more than MaxSize is undefined
     */
    void pop() {
        index = index == 0 ? MaxSize - 1 : (index - 1);
    }

    /**
     * @brief size: get MaxSize
     * @return MaxSize
     */
    size_t size() const {
        return MaxSize;
    }

    /**
     * @brief operator []
     * @param index
     * @return index of window as reference
     */
    T &operator[](size_t index) {
        return data[(this->index + index) % MaxSize];
    }
    /**
     * @brief operator []
     * @param index
     * @return index of window as reference
     */
    T &operator[](size_t index) const {
        return data[(this->index + index) % MaxSize];
    }

    /**
     * @brief front
     * @return window[0]
     */
    T &front() {
        return data[index];
    }
    /**
     * @brief front
     * @return window[0]
     */
    T &front() const {
        return data[index];
    }

    /**
     * @brief back
     * @return window[window.size() - 1]
     */
    T &back() {
        return data[(index + MaxSize - 1) % MaxSize];
    }
    /**
     * @brief back
     * @return window[window.size() - 1]
     */
    T &back() const {
        return data[(index + MaxSize - 1) % MaxSize];
    }

private:
    T data[MaxSize];
    size_t index;
};

/**
 * @brief The SlidingWindow class: a dynamically sized version
 * heap allocated btw
 */
template <typename T>
class SlidingWindow<T, 0>
{
public:
    /**
     * @brief SlidingWindow: create a sliding window with a dynamic size (less performant + takes more total space)
     * @param size
     */
    SlidingWindow(size_t size) : data(new T[size]), _size(size), index(0) {
        assert(size > 0);
    }

    SlidingWindow(const SlidingWindow &) = default;
    SlidingWindow(SlidingWindow &&) = default;

    /**
     * @brief ~SlidingWindow: delete the heap allocated array
     */
    ~SlidingWindow() {
        delete[] data;
    }

    /**
     * @brief push: push an item to the sliding window, if the window is full, the first element is overwritten
     * @param item
     */
    void push(T item) {
        data[index] = item;
        index = (index + 1) % _size;
    }
    /**
     * @brief pop: remove the last element of the window, popping more than MaxSize is undefined
     */
    void pop() {
        index = index == 0 ? _size - 1 : (index - 1);
    }

    /**
     * @brief size: get _size
     * @return _size
     */
    size_t size() const {
        return _size;
    }

    /**
     * @brief operator []
     * @param index
     * @return index of window as reference
     */
    T &operator[](size_t index) {
        return data[(this->index + index) % _size];
    }
    /**
     * @brief operator []
     * @param index
     * @return index of window as reference
     */
    T &operator[](size_t index) const {
        return data[(this->index + index) % _size];
    }

    /**
     * @brief front
     * @return window[0]
     */
    T &front() {
        return data[index];
    }
    /**
     * @brief front
     * @return window[0]
     */
    T &front() const {
        return data[index];
    }

    /**
     * @brief front
     * @return window[window.size() - 1]
     */
    T &back() {
        return data[(index + _size - 1) % _size];
    }
    /**
     * @brief front
     * @return window[window.size() - 1]
     */
    T &back() const {
        return data[(index + _size - 1) % _size];
    }

private:
    T *data;
    size_t index, _size;
};

}

#endif // SLIDINGWINDOW_H

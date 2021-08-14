.PHONY: gcc clang check

gcc:
	g++-11 --std=c++20 -Wall -Wextra -Wpedantic test.cpp

clang:
	clang++ --std=c++20 -Wall -Wextra -Wpedantic test.cpp

check:
	clang-tidy --extra-arg="--std=c++20" --header-filter=".*" --checks="clang-analyzer-*,cppcoreguidelines-*,misc-*,modernize-*,performance-*" test.cpp

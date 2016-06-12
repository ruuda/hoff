# Background

I’ve worked on several codebases with several source control systems. Big
repositories with more than a thousand active developers and ~300 commits per
day. Small repositories where all developers are in the same room. My personal
projects, where I am the sole developer apart from the occasional pull request.
I’ve worked in codebases with sophisticated review systems and testing
infrastructure. I’ve worked in codebases where there was no code review at all,
and no continuous integration. (Sometimes not even tests.) No one size fits all,
though some workflows are definitely better than others. Here I outline some of
my thoughs on collaborating on a software system.

## The Not Rocket Science Principle

I firmly believe in the [Not Rocket Science Principle][not-rocket-science]:

> Automatically maintain a repository of code that always passes all the tests.

This implies that you have a test suite in the first place. While I am generally
sceptic about tests (I think a strong type system can be much more valuable than
a big test suite) -- and I certainly don’t believe in test driven development --
I do believe in tests. Ideally, a passing test suite should give you enough
confidence to deploy or release the product.

[not-rocket-science]: https://graydon2.dreamwidth.org/1597.html

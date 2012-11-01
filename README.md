# Bloom filters for erlang

This is a simple implementation of a fixed size bloom filter given a desired false positive rate. The intended use case is when you know the upper bound of the amount keys you'll store, and this amount is fairly large (> 1,000,000).

As in [1], the bloom filter is implemented with the non-functional ```hipe_bifs:bitarray```, if you need a pure functional implementation checkout the ```pure``` branch (though this will me slower/produce a lot of garbage).

### Usage

```erlang
>>> BM = bloom_filter:new(10000000, 0.001).
>>> bloom_filter:is_element(test, BM).
false
>>> bloom_filter:is_element(test, bloom_filter:add_element(test, BM)).
true
```

If you need bloom filters with variable size checkout: [1] https://sites.google.com/site/scalablebloomfilters/

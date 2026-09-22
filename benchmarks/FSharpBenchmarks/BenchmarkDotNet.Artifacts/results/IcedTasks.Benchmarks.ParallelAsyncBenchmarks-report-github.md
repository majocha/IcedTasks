```

BenchmarkDotNet v0.13.9+228a464e8be6c580ad9408e98f18813f6407fb5a, Windows 11 (10.0.26200.9457)
Unknown processor
.NET SDK 11.0.100-rc.1.26425.128
  [Host]     : .NET 11.0.0 (11.0.26.42628), X64 RyuJIT AVX2 DEBUG
  DefaultJob : .NET 11.0.0 (11.0.26.42628), X64 RyuJIT AVX2

Categories=AsyncBindsLong  

```
| Method                                                   | Mean      | Error    | StdDev   | Ratio | RatioSD | Allocated | Alloc Ratio |
|--------------------------------------------------------- |----------:|---------:|---------:|------:|--------:|----------:|------------:|
| AsyncBuilder_async_long                                  | 155.76 ms | 0.238 ms | 0.211 ms |  1.00 |    0.00 |   7.88 KB |        1.00 |
| AsyncBuilder_async_long_applicative_overhead             | 157.97 ms | 3.081 ms | 3.297 ms |  1.02 |    0.02 |  10.06 KB |        1.28 |
| ParallelAsyncBuilderUsingStartChild_async_long           |  15.95 ms | 0.058 ms | 0.054 ms |  0.10 |    0.00 |  42.27 KB |        5.37 |
| ParallelAsyncBuilderUsingStartImmediateAsTask_async_long |  15.96 ms | 0.026 ms | 0.025 ms |  0.10 |    0.00 |  25.75 KB |        3.27 |

order:first L:"I"$read0 `$.z.x[0];
book:1 _ L;
iter:100;
-1 "order: ", string order;
-1 "book count: ", .Q.s1 (count book);

-1 "bench K [time]";
start:.z.p;
k) do[iter;r:-': order &+\ book]
elapsed:.z.p-start;
-1 "elapsed: ", .Q.s1 (`float$((`long$elapsed % iter) % 1000) % 1000);
-1 "filled: ", .Q.s1 r;

-1 "bench Q [time]";
start:.z.p;
do [iter;r:deltas order & sums book];
elapsed:.z.p-start;
-1 "elapsed: ", .Q.s1 (`float$((`long$elapsed % iter) % 1000) % 1000);
-1 "filled: ", .Q.s1 r;

exit 0;

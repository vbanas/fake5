for f in `ls solutions | grep good`; do
    mv "solutions/$f" "maybe_good_solutions/${f:0:${#f}-5}"
done


for n in {1..20}; 
do
    echo ""
    echo ""
    echo "RUNNING WITH $n PROCESSORS"
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
    cat test.xml | time ./xmlLong @mpl procs $n --
done

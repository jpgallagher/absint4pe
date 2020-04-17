#!/bin/sh

# $1 program to be specialised
# $2 goal to be specialised


LIB="/Users/jpg/ciao/build/bin"
PE="/Users/jpg/Research/LP/clptools/predabs/pe"

f=`basename $1`

# generate the property file
chclibs-qa "$1" -query "$2" -o "$f".qa.pl
chclibs-thresholds1 -prg "$f".qa.pl -a -o wut.props
chclibs-cpascc -prg "$f".qa.pl -cex "traceterm.out"  -withwut -wfunc h79 -o "$f".qa.cha.pl
$LIB/insertProps -prg "$1" -props "$f".qa.cha.pl -o "$f".sp
$PE/props1 -prg "$f".sp -entry "$2" -o "$f".props

#$PE/props1 -prg "$1" -entry "$2" -o "$f".props

# convert to logen representation
chc2logen "$1" "$f".logen
prop2logen "$f".props "$f".props.logen

# combine interpreter with input files
cat absint.pl.ann "$f".logen "$f".props.logen > absintProg.pl.ann

logengoal="go("$2")"

echo "$f"
echo "$logengoal"


# partial evaluation with logen
cogen  -np absintProg.pl  "$logengoal" > "$f".pe

# show the specialised program as a control flow graph

$PE/drawcfg -prg "$f".pe
dot -Tjpg -o "$f".pe.jpg cfg_graph.txt

rm -f "$f".logen "$f".props.logen *.gx *.cpx *.itf *.po cfg_graph.txt absintProg.pl.ann 
rm "$f".qa.pl "$f".qa.cha.pl "$f".sp "$f".props wut.props versions.out traceterm.out widenpoints


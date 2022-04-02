# hsell
an attempt at shell scripting in haskell

Shell lets you do fun stuff like

`ls -la | grep foo | wc -l` (I know, I know, grep -c, but it's hard to think of good random pipelines on the spot!)

but sometimes makes you do awful stuff like

`for x in $(seq 10); do echo $x; done`

or

`if [ $# -lt 5 ]; then echo "oh no!"; fi`

or

`x=$((x + 1))`

and the list goes on. Even worse, functions, data structures, modules, and other important programming niceties often don't quite match expectations. The typical solution to this problem is to switch to a more "programm-y" scripting language, like Python. But there, the beautiful pipelines turn into
```
p1 = subprocess.Popen(['ls', '-la'], stdout=subprocess.PIPE)
p2 = subprocess.Popen(['grep', 'foo'], stdin=p1.stdout, stdout=subprocess.PIPE)
p3 = subprocess.Popen(['wc', '-l'], stdin=p2.stdout)
```
which is nice and all, but it's no `ls -la | grep foo | wc -l'

Enter Haskell. I have a feeling that with Haskell's strengths in domain specific languages, it may be possible to make something that looks like
`($$!) "ls" $| grep "foo" $| "wc -l"`
and have some similarly terse syntax to handle other redirection/job-control primitives.

The best I have done so far is:

```haskell
runPipeline $ initPipeline `pipe` qProc "ls -la" `pipe` qProc "grep foo" `pipe` qProc "wc"
```

which feels like a decent start.

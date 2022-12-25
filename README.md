# aoc-2022

the code files are inside the aoc directory. 
all of these take input from stdin. 
my puzzle inputs are given in this directory. 

python is run by invoking the submodules from the root of the repository:
```bash
python -m py.d1 < ./1.txt
```

haskell solutions should be run with stack from the aoc subdirectory:
```bash
stack run d25 < ../25.txt
```
<!-- alternatively, you can compile with `ghc` for an order-of-magnitude speedup.
(if on Arch with `ghc` from the repositories, you'll need to specify -dynamic if missing module errors are thrown.) (-iaoc can be added to run from the repository root.) -->

## summary

a essence of each day's problem.

1. maximum of sum of number groups, then top 3.
2. rock paper scissors pattern matching, then inverting.
3. intersections of characters in a string, then between 3 strings. 
4. 1D interval containment, then interval overlap. 
5. stack manipulations. the problem was easy, but the input was just not easy to parse...
6. sliding window of a string, then larger sliding window. 
7. **(fun!)** parsing directory tree listing, then calculating sizes and free space. this was good fun, and my solution has a nice stateful parser.
8. line of sight for 2d height map, then line of sight from each point. part 2 was particularly slow because it did not follow from part 1.
9. rope simulation, then longer rope.
10. simple opcode execution, then drawing pixels.
11. simulating machines and work being passed between them according to arithmetic rules, then long-term simulation. very subtle bug with Haskell's fromListWith order.
12. pathfinding around heightmap, then multiple starting points.
13. pairwise comparison of nested list objects, then sorting them.
14. falling sand simulation, then larger scale with floor. 
15. overlapping diamond regions, then finding uncovered point. 
16. **(hard)** valve opening sequence to maximise flow, then with helper.
17. tetris falling rocks simulation, then long term. long-term required looking for cycles.
18. surface area of voxel shape, then inner contained volume.
19. **(hard)** optimal planning of robot purchases, then over longer time frame.
20. cyclic list manipulation, then multiple iterations.
21. evaluation of algebraic expression, then inverting to find solution. usually uses topo-sort, but Haskell does that automatically. z3 used to solve. 
22. **(interesting!)** following given path around map, then with map as surface of a cube.
23. simulation of cellular system, then until stable state.
24. pathfinding around grid with moving obstacles, then pathfinding back and there again.
25. conversions of a base-5 number system with negative coefficients.

## results 

```fortran
      --------Part 1--------   --------Part 2--------
Day       Time   Rank  Score       Time   Rank  Score
 25      35:20   1557      0      35:28   1310      0
 24   01:08:04   1243      0   01:13:33   1096      0
 23      30:45    386      0      35:17    408      0
 22      50:13   1153      0   01:27:49    205      0
 21      16:15   1632      0      59:43   1809      0
 20      35:49    779      0      53:00    951      0
 19       >24h  12308      0       >24h  11303      0
 18   06:04:06  10272      0   06:31:38   7220      0
 17      49:35    949      0   02:06:48   1240      0
 16       >24h  14926      0       >24h  12973      0
 15   06:55:18  14660      0   07:11:42   9330      0
 14   02:08:09   7777      0   02:22:14   7249      0
 13   01:32:30   5908      0   01:37:40   5230      0
 12   04:10:44  11662      0   04:20:27  11159      0
 11      24:29    991      0      27:59    445      0
 10   02:29:19  14509      0   02:39:44  12282      0
  9   04:10:25  20917      0   04:31:15  15350      0
  8   09:36:21  43771      0       >24h  62226      0
  7   03:29:48  17376      0   03:32:37  16016      0
  6   04:25:05  44088      0   04:26:36  42791      0
  5   01:55:52  17515      0   01:57:17  16388      0
  4   06:18:24  47853      0   06:23:53  46029      0
  3   05:49:21  42964      0   05:52:25  38005      0
  2      05:21    496      0      08:39    456      0
  1      01:39    235      0      02:14    130      0
```

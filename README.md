# sts_map_oracle
Pitch-perfect copy of map generation algorithm from Slay the Spire

# Usage

Prints out map layouts in console for given seed:
```
sts_map_oracle.exe --seed 673465884448
```
To save maps in given directory in JSON format:
```
sts_map_oracle.exe --seed 673465884448 --path C:\maps\
```

# Download
https://github.com/Ru5ty0ne/sts_map_oracle/releases/

# Output example

```
Act 1


14     R  R        R     R
         \|      / |   /
13        ?     M  M  M
            \     \  \  \
12           R     M  E  ?
           /   \   |  |/
11        ?     ?  E  ?
          |     |  |  | \
10        M     M  R  $  R
          |   /      \|  |
9         M  R        M  E
          |  |        |/ |
8         T  T        T  T
          |  |      / |  |
7         M  E     M  ?  M
          |  |       \| \|
6         E  M        M  ?
        /    |      /   \|
5      R     E     M     E
         \     \ /     / |
4         M     ?     M  ?
          |     | \ /  / |
3         M     M  M  M  ?
        /     /  /  /  /
2      $     M  ?  $  ?
         \     \|/  /
1         M     M  ?
        /     /      \
0      M     M        M


Act 2


14     R  R  R  R
       |/   \|/
13     E     $
       | \ / | \
12     ?  M  ?  M
       |  | \|    \
11     ?  M  ?     E
       |/    | \ /
10     M     ?  M
       | \ /      \
9      E  M        R
       |  |      / |
8      T  T     T  T
       | \| \ /      \
7      R  E  R        R
       |/ |/            \
6      $  M              E
       | \| \            |
5      E  R  E           R
       |/    | \       /
4      M     M  $     M
       | \     \  \ /
3      ?  M     ?  ?
       | \|   /    | \
2      ?  M  ?     M  M
         \| \|   /      \
1         M  ?  ?        M
        /  /      \    /
0      M  M        M  M


Act 3


14     R  R        R  R
       |  |      /    | \
13     ?  E     ?     M  M
       |/     /       |/
12     $     R        E
       | \ /          | \
11     E  M           M  M
       |  | \       /  /
10     M  E  M     ?  ?
         \|  |     |  |
9         ?  M     ?  ?
        / |  |     |    \
8      T  T  T     T     T
       | \  \  \ /     /
7      R  R  $  M     ?
       |  |/    |   /
6      E  M     R  E
       |/   \ /   \  \
5      R     E     E  R
       | \     \     \  \
4      ?  M     M     ?  M
       |  |     | \ /  /
3      M  ?     ?  M  M
         \|       \| \  \
2         M        M  M  $
          |      / |    \|
1         M     M  ?     M
        /     /      \ /
0      M     M        M
```

```php
echo ("https://${bar}$a");
```

```
T3 = ROPE_INIT 3 string("https://")
T3 = ROPE_ADD 1 T3 CV0($bar)
T2 = ROPE_END 2 T3 CV1($a)
ECHO T2
```

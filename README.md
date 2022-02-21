<a id="x-28HUMANIZE-DURATION-3A-40README-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

# Duration representation for humans!

This is a small library useful for time duration humanization.

<a id="humanize-duration-asdf-system-details"></a>

## HUMANIZE-DURATION ASDF System Details

* Description: Provides [`humanize-duration:humanize-duration`][74ec] function to make readable representation of `LOCAL-TIME-DURATION:DURATION` objects.

* Licence: Unlicense

* Author: Alexander Artemenko

* Homepage: [https://40ants.com/humanize-duration/][a0c4]

* Source control: [GIT][9168]

<a id="x-28HUMANIZE-DURATION-3A-3A-40INTRO-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Introduction

It is different from `LOCAL-TIME-DURATION:HUMAN-READABLE-DURATION`, because allows
to output only significant parts of a duration object.

Also it limits a number of part. For example, if there days, hours, minutes and seconds,
then most probably it is already not important exactly how many minutes and seconds
passed since the moment.

Compare these two examples:

<a id="local-time-duration-human-readable-duration"></a>

### local-time-duration:human-readable-duration

```
CL-USER> (local-time-duration:human-readable-duration
          (local-time-duration:duration :day 5
                                        :hour 13
                                        :minute 0
                                        :sec 14
                                        :nsec 10042))
" 5 days 13 hours 14 seconds 10042 nsecs"
```
<a id="humanize-duration-humanize-duration"></a>

### humanize-duration:humanize-duration

```
CL-USER> (humanize-duration:humanize-duration
          (local-time-duration:duration :day 5
                                        :hour 13
                                        :minute 0
                                        :sec 14
                                        :nsec 10042))
"5 days 13 hours"
```
Main job is done at `humanize-duration` ([`1`][74ec] [`2`][ffe3]):

<a id="x-28HUMANIZE-DURATION-3AHUMANIZE-DURATION-20FUNCTION-29"></a>

### [function](9d51) `humanize-duration:humanize-duration` duration &key stream (n-parts 2) (format-part #'default-format-part)

This is the better version of `LOCAL-TIME-DURATION:HUMAN-READABLE-DURATION`.

By default it returns only 2 most significant duration parts.

If duration is 2 hour, 43 seconds and 15 nanoseconsds, then
function will return "2 hours 43 seconds":

```lisp
CL-USER> (ultralisp/utils/time:humanize-duration
          (local-time-duration:duration :hour 2
                                        :sec 43
                                        :nsec 15))
"2 hours 43 seconds"
```
Also, you can pass a `:format-part` argument.
It should be a function of three arguments:
`(stream part-type part)` where `part-type` is a keyword
from this list:

```lisp
(list :weeks :days :hours :minutes :secs :nsecs)
```
`humanize-duration` ([`1`][74ec] [`2`][ffe3]) accepts `:FORMAT-PART` argument, which is [`default-format-part`][d084] function by default:
your own version. This could be useful if you want to support localization to other languages.

<a id="x-28HUMANIZE-DURATION-3ADEFAULT-FORMAT-PART-20FUNCTION-29"></a>

### [function](c5a0) `humanize-duration:default-format-part` stream part-type part

This is should return a string with propertly pluralized form.

* `PART-TYPE` argument is a member of (list :weeks :days :hours :minutes :secs :nsecs).

* `PART` is an integer.

Here are possible results:

```text
(t :weeks 1) -> "1 week"
(t :weeks 5) -> "5 weeks"
(t :day 2) -> "2 days"
```
Here is how you can localize output for your language

<a id="x-28HUMANIZE-DURATION-3A-3A-40LOCALIZATION-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

## Localization

`humanize-duration` ([`1`][74ec] [`2`][ffe3]) comes with predefined Russian localization.

<a id="x-28HUMANIZE-DURATION-2FRU-3A-3A-40INDEX-2040ANTS-DOC-2FLOCATIVES-3ASECTION-29"></a>

### Russian localization

This package includes a single function, useful to display duration in Russian language:

<a id="x-28HUMANIZE-DURATION-2FRU-3AFORMAT-PART-20FUNCTION-29"></a>

#### [function](18f0) `humanize-duration/ru:format-part` stream part-type part

This is Russian version of part formatter for [`humanize-duration`][ffe3]

You can use it as an template, to define other languages.

Please, don't forget to contribute your solutions!

Here is how you can use it in your code:

```
CL-USER> (humanize-duration:humanize-duration
          (local-time-duration:duration :day 5
                                        :hour 13
                                        :minute 0
                                        :sec 14
                                        :nsec 10042)
          :format-part #'humanize-duration/ru:format-part)
"5 дней 13 часов"
```
Here is how [`format-part`][958e] function is defined in the code:

```
(defun format-part (stream part-type part)
  "This is Russian version of part formatter for HUMANIZE-DURATION"
  (format stream "~d ~A"
          part
          (apply #'choose-form
                 part
                 (ecase part-type
                   (:weeks '("неделя" "недели" "недель"))
                   (:days '("день" "дня" "дней"))
                   (:hours '("час" "часа" "часов"))
                   (:minutes '("минута" "минуты" "минут"))
                   (:secs '("секунда" "секунды" "секунд"))
                   (:nsecs '("наносекунда" "наносекунды" "наносекунд"))))))

```
Russian version uses internal helper, to choose a correct word form:

```
(defun choose-form (n &rest forms)
  "This function is based on this gettext formula:

   ```
   Plural-Forms: nplurals=3; plural=n%10==1 && n%100!=11 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2;
   ```
"
  (let ((n%10 (rem n 10))
        (n%100 (rem n 100)))
    (cond
      ((and (= n%10
               1)
            (not (= n%100
                    11)))
       (first forms))
      ((and (>= n%10
                2)
            (<= n%10
                4)
            (or (<= n%100
                    10)
                (>= n%100
                    20)))
       (second forms))
      (t
       (third forms)))))

```
Applied to a different numbers it produces the following:

```
CL-USER> (flet ((p (n)
                  (format t "~A ~A~%"
                          n
                          (choose-form n "яблоко" "яблока" "яблок"))))
           (loop for i upto 12
                 do (p i)))
0 яблок
1 яблоко
2 яблока
3 яблока
4 яблока
5 яблок
6 яблок
7 яблок
8 яблок
9 яблок
10 яблок
11 яблок
12 яблок
```

[a0c4]: https://40ants.com/humanize-duration/
[ffe3]: https://40ants.com/humanize-duration/#x-28-23A-28-2817-29-20BASE-CHAR-20-2E-20-22humanize-duration-22-29-20ASDF-2FSYSTEM-3ASYSTEM-29
[958e]: https://40ants.com/humanize-duration/#x-28HUMANIZE-DURATION-2FRU-3AFORMAT-PART-20FUNCTION-29
[d084]: https://40ants.com/humanize-duration/#x-28HUMANIZE-DURATION-3ADEFAULT-FORMAT-PART-20FUNCTION-29
[74ec]: https://40ants.com/humanize-duration/#x-28HUMANIZE-DURATION-3AHUMANIZE-DURATION-20FUNCTION-29
[9168]: https://github.com/40ants/humanize-duration
[9d51]: https://github.com/40ants/humanize-duration/blob/f7cfd19c35a1e5393f6ed7f59abaa60cb19861d2/src/core.lisp#L116
[c5a0]: https://github.com/40ants/humanize-duration/blob/f7cfd19c35a1e5393f6ed7f59abaa60cb19861d2/src/core.lisp#L95
[18f0]: https://github.com/40ants/humanize-duration/blob/f7cfd19c35a1e5393f6ed7f59abaa60cb19861d2/src/ru.lisp#L98

* * *
###### [generated by [40ANTS-DOC](https://40ants.com/doc/)]

function quick_sort (start, end) {
  const pivot = a[Math.floor((start + end) / 2)];
  let left = start;
  let right = end;

  while (true) {
    while (a[left] < pivot) left += 1;
    while (pivot < a[right]) right -= 1;
    if (right <= left) break;
    const tmp = a[left];
    a[left] = a[right];
    a[right] = tmp;
    left += 1;
    right -= 1;
  }

  if (start < left - 1) quick_sort(start, left - 1);
  if (right + 1 < end) quick_sort(right + 1, end);
}

var a = [17, 1, 11, 10, 5, 19, 16, 3, 7, 0, 7, 14, 14, 12, 0];
quick_sort(0, a.length - 1);
console.log(a);
a
function quick_sort(start, end){
  var pivot = a[Math.floor((start + end) / 2)];
  var left = start;
  var right = end;

  while (true) {
    while (a[left] < pivot) left += 1;
    while (pivot < a[right]) right -= 1;
    if(right <= left) break;
    var tmp = a[left];
    a[left] = a[right];
    a[right] = tmp;
    left += 1;
    right -= 1;
  }

  if(start < left - 1) quick_sort(start,left - 1);
  if(right + 1 < end) quick_sort(right + 1,end);
}

var a = [];
for(var i = 0; i < 15; i += 1) a.push(Math.floor(Math.random() * 20));
console.log(a);

quick_sort(0, a.length-1);
console.log(a);

let count = 10
module.exports = {
  countup: x => {
    count += x
  },
  count: () => {
    return count
  }
}

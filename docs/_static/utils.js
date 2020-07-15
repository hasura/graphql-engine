const hasuraUtils = {
  formatNumber: function(number) {
    if (typeof number !== "number") return number;
    
    const SIsymbol = ["", "k", "M", "G", "T", "P", "E"];
    const absNumber = Math.abs(number);
    const sign = Math.sign(number);

    // what tier? (determines SI symbol)
    const tier = Math.log10(absNumber) / 3 | 0;

    // if zero, we don't need a suffix
    if (tier === 0) return sign * absNumber;

    // get suffix and determine scale
    const suffix = SIsymbol[tier];
    const scale = Math.pow(10, tier * 3);

    // scale the number
    const scaled = absNumber / scale;

    // format number and add suffix
    return sign * scaled.toFixed(1) + suffix;
  }
}

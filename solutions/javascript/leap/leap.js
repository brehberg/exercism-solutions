export const isLeap = (year) => {
  return new Year(year).isLeapYear();
};

class Year {
  constructor(number) {
    this.year = number;
  }
  isLeapYear() {
    return this.divisibleBy(4) && 
      (this.divisibleBy(400) || !this.divisibleBy(100));
  }
  divisibleBy(number) {
    return (this.year % number === 0);
  }    
}

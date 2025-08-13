data class Year(val year: Int) {    
    val isLeap: Boolean = if (year % 100 == 0) year % 400 == 0 else year % 4 == 0
}

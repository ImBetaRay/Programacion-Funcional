/*
 Write a class BankAccount with methods deposit and withdraw , and a read-only
property balance 
*/
class BankAccount() {
    private var balance = 0.0;

    def deposit(amount : Double) = {
        balance += amount
    }

    def withdraw(amount : Double) = {
        balance -= amount
    }
    
}

/*
 Write a class Time with read-only properties hours and minutes and a method
 before(other: Time): Boolean that checks whether this time comes before the
 other. A Time object should be constructed as new Time(hrs, min) , where hrs is in
 military time format (between 0 and 23).
*/
class Time(private var hrs : Int, private var min : Int){
    min = {
        if (min % 60 == 0) {
            hrs += min/60
            0
        } else if (min < 0) {
            hrs += (min / 60 - 1)
            60 + min % 60
        } else if (min > 59) {
            hrs += min / 60
            min % 60
        } else {
            min
        }
    }
    
    hrs = {
        if (hrs < 0) {
            24 + (hrs % 24)
        } else if (hrs > 23) {
            hrs % 24
        } else { 
            hrs
        }
    }

    def hours = hrs
    def minutes = min

    def before(other : Time) = {
        hours < other.hours || (other.hours == hours && minutes < other.minutes)  
    }
}



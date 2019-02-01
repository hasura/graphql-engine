var log = true;

function logThis (title, message) {
    if (log) {
        if (title === undefined && message === undefined) {
            console.log('Something was logged')
        }else if (message === undefined) {
            console.log(`${title} -> ${message}`)
        }else if (title === undefined) {
            console.log(`${title} -> ${message}`)
        }else {
            console.log(`${title} -> ${message}`)
        }
        
    }
}

module.exports = logThis
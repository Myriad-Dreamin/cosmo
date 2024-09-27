
import fs from 'fs'

const testBase = 'packages/cosmo/src/test/scala/cosmo/TestBase.scala'

const content = fs.readFileSync(testBase, 'utf8')

const guards = [
    'showResult = false',
    'syntaxOnly = false',
    'evalOnly = false',
    'compileCpp = false',
    'updateSnapshot = false'
]

let checkFailed = false
for (const guard of guards) {
    const re = new RegExp(guard, 'g')
    if (!re.test(content)) {
        console.log(`Missing guard: ${guard}`)
        checkFailed = true
    }
}

if (checkFailed) {
    process.exit(1)
}

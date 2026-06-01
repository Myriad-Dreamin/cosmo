import { existsSync, readFileSync, readdirSync } from 'fs'
import { join } from 'path'

const requiredDocs = [
  'spec.typ',
  'type.typ',
  'class.typ',
  'expr.typ',
  'control-flow.typ',
  'std.typ',
  'runtime.typ',
  'package.typ',
  'macro-expr.typ',
  'compile-time-evaluation.typ',
  'testing.typ',
]

const docRoot = join('docs', 'cosmo')
const errors = []

function read(path) {
  return readFileSync(path, 'utf8')
}

function requireFile(path) {
  if (!existsSync(path)) {
    errors.push(`missing required file: ${path}`)
    return false
  }
  return true
}

for (const file of requiredDocs) {
  requireFile(join(docRoot, file))
}

for (const file of requiredDocs) {
  const path = join(docRoot, file)
  if (!existsSync(path)) {
    continue
  }
  const content = read(path)
  if (!content.includes('== Examples')) {
    errors.push(`${path} must include an "== Examples" section`)
  }
  if (!content.includes('```')) {
    errors.push(`${path} must include at least one fenced example block`)
  }
}

const requiredSnippets = new Map([
  [
    'spec.typ',
    [
      'Subset Boundary',
      'Conformance Overview',
      'Stage 1 Capability Profile',
      'validate-cosmo1-stage1-capability-profile',
    ],
  ],
  [
    'std.typ',
    [
      'Capability Identifiers',
      'Stage 1 Capability Profile',
      'Future descriptor/std proposals must name',
      'validate-cosmo1-stage1-capability-profile',
    ],
  ],
  [
    'package.typ',
    [
      'Stage Validation',
      'Stage 1 Capability Profile',
      'validate-cosmo1-stage1-capability-profile',
    ],
  ],
  [
    'testing.typ',
    [
      'Bug/spec Sync Rule',
      'Spec Impact Review Rule',
      'node scripts/validateCosmo0Docs.js',
    ],
  ],
])

for (const [file, snippets] of requiredSnippets) {
  const path = join(docRoot, file)
  if (!existsSync(path)) {
    continue
  }
  const content = read(path)
  for (const snippet of snippets) {
    if (!content.includes(snippet)) {
      errors.push(`${path} must include "${snippet}"`)
    }
  }
}

const changeRoot = join('openspec', 'changes')
const stagedRuntimeName = /^(add-core0-|add-cosmo0-|narrow-cosmo0-)/
const specImpactText = /docs\/cosmo\/[A-Za-z0-9-]+\.typ|implementation-only|implementation only/i

for (const entry of readdirSync(changeRoot, { withFileTypes: true })) {
  if (!entry.isDirectory() || entry.name === 'archive') {
    continue
  }
  if (!stagedRuntimeName.test(entry.name)) {
    continue
  }

  const changeDir = join(changeRoot, entry.name)
  const parts = []
  for (const file of ['proposal.md', 'design.md', 'tasks.md']) {
    const path = join(changeDir, file)
    if (existsSync(path)) {
      parts.push(read(path))
    }
  }

  const combined = parts.join('\n')
  if (!specImpactText.test(combined)) {
    errors.push(
      `${changeDir} must reference changed docs/cosmo/*.typ files or justify implementation-only status`,
    )
  }
}

if (errors.length > 0) {
  console.error('cosmo0 docs validation failed:')
  for (const error of errors) {
    console.error(`- ${error}`)
  }
  process.exit(1)
}

console.log(`cosmo0 docs validation passed (${requiredDocs.length} required docs with examples checked)`)

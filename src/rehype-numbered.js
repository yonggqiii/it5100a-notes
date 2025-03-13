const rehypeNumbered = (options = { refName: {} }) => {
  return async (tree, file) => {
    let n = { n : 1 }
    let refs = { }
    handleTree(tree, n, options.refName, refs)
    handleTree2(tree, n, options.refName, refs)
  }
}

const handleTree = (ele, n, refNames, refs) => {
  if (ele.type === 'element' && ele.tagName === 'theorem') {
    let eleId = ele.properties.id
    if (eleId && !(eleId in refs)) {
      // if there is already a name, use it
      if (ele.properties.name) {
        refs[eleId] = ele.properties.name
      } else {
        let prefix = eleId.split(':')[0]
        if (prefix in refNames) {
          let name = `${refNames[prefix]} ${n.n}`
          ele.properties.name = name
          refs[eleId] = name
          n.n += 1
        }
      }
    }
  }
  if (ele.children) {
    ele.children.forEach(x => handleTree(x, n, refNames, refs));
  }
}

const handleTree2 = (ele, n, refNames, refs) => {
  if (ele.type === 'element' && ele.tagName === 'autoref') {
    let eleId = ele.properties.id
    if (eleId && eleId in refs) {
      ele.tagName = 'a'
      ele.properties = { href: `#${eleId}` }
      ele.children.push({ type: 'text', value: refs[eleId] })
    }
  }
  if (ele.children) {
    ele.children.forEach(x => handleTree2(x, n, refNames, refs));
  }
}

export default rehypeNumbered;

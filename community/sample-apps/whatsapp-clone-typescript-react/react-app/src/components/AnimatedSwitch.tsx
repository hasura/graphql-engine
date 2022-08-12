import styled from 'styled-components'
import { AnimatedSwitch, spring } from 'react-router-transition'

const glide = val =>
  spring(val, {
    stiffness: 174,
    damping: 24,
  })

const mapStyles = styles => ({
  transform: `translateX(${styles.offset}%)`,
})

export default styled(AnimatedSwitch).attrs(() => ({
  atEnter: { offset: 100 },
  atLeave: { offset: glide(-100) },
  atActive: { offset: glide(0) },
  mapStyles,
}))`
  position: relative;
  overflow: hidden;
  width: 100%;
  height: 100%;
  > div {
    position: absolute;
    overflow: hidden;
    width: 100%;
    height: 100%;
  }
`

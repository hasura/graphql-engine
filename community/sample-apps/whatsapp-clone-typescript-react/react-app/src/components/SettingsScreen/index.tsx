import * as React from 'react'
import { Suspense } from 'react'
import { RouteComponentProps } from 'react-router-dom'
import Navbar from '../Navbar'
import SettingsForm from './SettingsForm'
import SettingsNavbar from './SettingsNavbar'

export default ({ history }: RouteComponentProps) => (
  <div className="SettingsScreen Screen">
    <Navbar>
      <SettingsNavbar history={history} />
    </Navbar>
    <Suspense fallback={null}>
      <SettingsForm />
    </Suspense>
  </div>
)

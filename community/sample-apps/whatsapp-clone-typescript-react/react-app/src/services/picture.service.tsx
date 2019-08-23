import { getAuthHeader } from './auth.service'

export const pickPicture = () => {
  return new Promise((resolve, reject) => {
    const input = document.createElement('input')
    input.type = 'file'
    input.accept = 'image/*'
    input.onchange = e => {
      const target = e.target as HTMLInputElement
      resolve(target.files[0])
    }
    input.onerror = reject
    input.click()
  })
}

export const uploadProfilePicture = file => {
  const formData = new FormData()
  formData.append('file', file)
  formData.append('upload_preset', 'profile-pic')

  return fetch(`${process.env.REACT_APP_AUTH_URL}/upload-profile-pic`, {
    method: 'POST',
    body: formData,
    headers: {
      Authorization: getAuthHeader(),
    }
  }).then(res => {
    return res.json()
  })
}

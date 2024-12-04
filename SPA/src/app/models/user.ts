export interface RegisterUser {
    name: string,
    email: string,
    phone: string,
    password: string
}

export interface ConfirmRegisterUser {
    token: string,
    email: string
}

export interface EditUser {
    email: string,
    nameToEdit: string,
    emailToEdit: string,
    phoneNumberToEdit: string,
}

export interface ConfirmEditUser {
    token: string,
    email: string,
    emailToEdit: string,
    phoneNumberToEdit: string
}

export interface User {
    email: string,
    password: string
}

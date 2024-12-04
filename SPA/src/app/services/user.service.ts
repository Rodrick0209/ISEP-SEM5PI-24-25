import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';
import { ConfirmEditUser, ConfirmRegisterUser, EditUser, RegisterUser } from '../models/user';

export interface User {
  email: string;
}

@Injectable({
  providedIn: 'root'
})

export class UserService {
  private urlEmail = '/api/users/email';
  private url = '/api/users/patients';
  private urlConfirm = '/api/users/patients/confirm'
  private editUrl = '/api/users/patients/edit'
  private editUrlConfirm = '/api/users/patients/edit/confirm'
  private deleteUrl = '/api/users/patients/delete'
  private deleteUrlConfirm =  '/api/users/patients/delete/confirm'

  constructor(private http: HttpClient) { }

  getUserByEmail(email: string): Observable<User> {
    return this.http.get<User>(`${this.urlEmail}/${email}`);
  }

  register(user: RegisterUser) : Observable<any> {
    const body = {
      name: user.name,
      email: user.email,
      phoneNumber: user.phone,
      password: user.password
    }
    return this.http.post(this.url, body);
  };

  confirmRegistration(confirmation: ConfirmRegisterUser): Observable<any> {
    const url = `${this.urlConfirm}?token=${confirmation.token}&email=${confirmation.email}`;

    return this.http.post(url, {});
  };

  edit(user: EditUser): Observable<any> {
    const body: any = {}
    if (user.email) body.email = user.email;
    if (user.nameToEdit) body.nameToEdit = user.nameToEdit;
    if (user.emailToEdit) body.emailToEdit = user.emailToEdit;
    if (user.phoneNumberToEdit) body.phoneNumberToEdit = user.phoneNumberToEdit;

    return this.http.patch(this.editUrl, body);
  }

  confirmEdit(confirmation: ConfirmEditUser): Observable<any> {
    const url = `${this.editUrlConfirm}?token=${confirmation.token}&email=${confirmation.email}&emailToEdit=${confirmation.emailToEdit}&phoneNumberToEdit=${confirmation.phoneNumberToEdit}`;

    return this.http.patch(url, {});
  }


  forgotPassword(email: string): Observable<any> {
    return this.http.post('/api/Users/Forgot-Password', { email });
  }

  resetPassword(token: string, newPassword: string, email: string): Observable<any> {
    return this.http.post('/api/Users/Reset-Password', { token, newPassword, email });
  }

  delete(email: string): Observable<any> {
    const url = `${this.deleteUrl}/${email}`;

    return this.http.delete(url);
  }

  confirmDelete(token: string, email: string): Observable<any>{
    const url = `${this.deleteUrlConfirm}?token=${token}&email=${email}`;

    return this.http.delete(url);
  }



}

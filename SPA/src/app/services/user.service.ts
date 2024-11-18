import { HttpClient, HttpParams } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { AuthService } from './auth.service';

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

  constructor(private http: HttpClient) { }

  getUserByEmail(email: string): Observable<User> {
    return this.http.get<User>(`${this.urlEmail}/{email}`);
  }

  register(name: string, email: string, phone: string, password: string) : Observable<any> {
    const body = {
      name: name,
      email: email,
      phoneNumber: phone,
      password: password
    }
    return this.http.post(this.url, body);
  };

  confirmRegistration(token: string, email: string): Observable<any> {
    const url = `${this.urlConfirm}?token=${token}&email=${email}`;

    return this.http.post(url, {});
  };

  edit(email: string, name: string, newEmail: string, phone: string): Observable<any> {
    const body: any = {}
    if (email) body.email = email;
    if (name) body.nameToEdit = name;
    if (newEmail) body.emailToEdit = newEmail;
    if (phone) body.phoneNumberToEdit = phone;

    return this.http.patch(this.editUrl, body);
  }

  confirmEdit(token: string, email: string, newEmail: string, phone: string): Observable<any> {
    const url = `${this.editUrlConfirm}?token=${token}&email=${email}&emailToEdit=${newEmail}&phoneNumberToEdit=${phone}`;

    return this.http.patch(url, {});
  }


  forgotPassword(email: string): Observable<any> {
    return this.http.post('/api/Users/Forgot-Password', { email });
  }

  resetPassword(token: string, newPassword: string, email: string): Observable<any> {
    return this.http.post('/api/Users/Reset-Password', { token, newPassword, email });
  }



}

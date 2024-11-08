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
  private url = '/api/users/patients';
  private urlConfirm = '/api/users/patients/confirm'
  private editUrl = '/api/users/patients/edit'
  private editUrlConfirm = '/api/users/patients/edit/confirm'

  constructor(private http: HttpClient, private authService: AuthService) { }

  getUserByEmail(email: string): Observable<User> {
    return this.http.get<User>(`${this.url}?email=${email}`);
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

    return this.http.get(url);
  };

  edit(email: string, name: string, newEmail: string, phone: string): Observable<any> {
    const body: any = {}
    if (email) body.email = email;
    if (name) body.nameToEdit = name;
    if (newEmail) body.emailToEdit = newEmail;
    if (phone) body.phoneNumberToEdit = phone;

    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.patch(this.editUrl, body, { headers });
  }

  confirmEdit(token: string, email: string, newEmail: string, phone: string): Observable<any> {
    const url = `${this.editUrlConfirm}?token=${token}&email=${email}&emailToEdit=${newEmail}&phoneNumberToEdit=${phone}`;

    const headers = { 'Authorization': 'Bearer ' + this.authService.getToken() };
    return this.http.patch(url, { headers });
  }
}

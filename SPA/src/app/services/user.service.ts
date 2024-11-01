import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class UserService {
  private url = '/api/users/patients';
  private urlConfirm = '/api/users/patients/confirm'

  constructor(private http: HttpClient) { }

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
}

import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class RegisterService {
  private url = 'http://localhost:5000/api/users/patients'

  constructor(private http: HttpClient) { }

  register(name: string, email: string, phone: string, password: string) : Observable<any> {
    const body = {
      name: name,
      email: email,
      phoneNumber: phone,
      password: password
    }
    return this.http.post(this.url, body);
  }
}

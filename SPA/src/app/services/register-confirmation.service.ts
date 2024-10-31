import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class RegisterConfirmationService {
  private url = "http://localhost:5000/api/users/patients/confirm"

  constructor(private http: HttpClient) { }
  confirmRegistration(token: string, email: string): Observable<any> {
    const url = `${this.url}?token=${token}&email=${email}`;

    return this.http.get(url);
  };
}

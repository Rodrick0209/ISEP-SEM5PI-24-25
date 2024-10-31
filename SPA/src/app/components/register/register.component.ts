import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { HttpClientModule, HttpClient } from '@angular/common/http';
import { Router } from '@angular/router';

@Component({
  standalone: true,
  imports: [FormsModule, CommonModule, HttpClientModule], // Add HttpClientModule to imports
  templateUrl: './register.component.html',
  styleUrl: './register.component.css'
})
export class RegisterComponent {
  name: string = '';
  email: string = '';
  phone: string = '';
  password: string = '';
  errorMessage: string | null = null;
  successMessage: string | null = null;

  constructor(private http: HttpClient, private router: Router) {}

  onRegister() {
    // Create a new user object
    const newUser = {
      name: this.name,
      email: this.email,
      phoneNumber: this.phone,
      password: this.password
    };

    // Send a POST request to your backend API
    this.http.post('/api/Users', newUser).subscribe(
      (response: any) => {
        console.log('Registration successful:', response);
        // Redirect to login or another page on successful registration
        this.successMessage = 'A confirmation email has been sent. Please check your inbox.'; // Set success message
      },
      (error) => {
        console.error('Registration failed:', error);
        this.errorMessage = 'Registration failed. Please try again.';
      }
    );
  }
}

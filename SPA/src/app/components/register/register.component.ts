import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { UserService } from '../../services/user.service';

@Component({
  standalone: true,
  imports: [FormsModule, CommonModule], // Add HttpClientModule to imports
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

  constructor(private userService: UserService) { }

  onRegister() {
    this.userService.register(this.name, this.email, this.phone, this.password).subscribe(
      (response) => {
        console.log('Registration successful:', response);
        this.successMessage = 'Registration successful! Please check your email to confirm your account.';
      },
      (error) => {
        console.log('Error:', error);
        this.errorMessage = 'An error occurs when registering the user: ' + error.error.message;
      }
    );
  }
}

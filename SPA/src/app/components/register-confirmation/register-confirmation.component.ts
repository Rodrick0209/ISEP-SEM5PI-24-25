import { HttpClient } from '@angular/common/http';
import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { UserService } from '../../services/user.service';

@Component({
  selector: 'app-confirmation',
  standalone: true,
  imports: [],
  templateUrl: './confirmation.component.html',
  styleUrl: './confirmation.component.css'
})
export class RegisterConfirmationComponent implements OnInit{
  private apiUrl = 'http://localhost:5000/api/users/patients/confirm';
  token: string | null = null;
  email: string | null = null;

  constructor(private userService: UserService, private route: ActivatedRoute, private router: Router) {}

  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.token = params['token'];
      this.email = params['email'];
      this.confirmRegistration();
    });
  }

  confirmRegistration() {
    if (this.token && this.email) {
      this.userService.confirmRegistration(this.token, this.email).subscribe({
        next: (response) => {
          // Handle success
          this.router.navigate(['/confirmation-success'], {
            queryParams: { message: 'Your account has been confirmed succesfully.' }
          });
        },
        error: (error) => {
          // Handle error
          this.router.navigate(['/confirmation-error'], {
            queryParams: { message: error.error.message || 'An error occurred.' }
          });
        }
      });
    } else {
      this.router.navigate(['/confirmation-error'], {
        queryParams: { message: 'Invalid confirmation link.' }
      });
    }
  }
}

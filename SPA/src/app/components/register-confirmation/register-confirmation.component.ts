import { Component, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { UserService } from '../../services/user.service';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-register-confirmation',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './register-confirmation.component.html',
  styleUrl: './register-confirmation.component.css'
})
export class RegisterConfirmationComponent implements OnInit{
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
      var confirmRegistration = {
        token: this.token,
        email: this.email
      };
      this.userService.confirmRegistration(confirmRegistration).subscribe({
        next: (response) => {
          // Handle success
          console.log('Account confirmed:', response);
          this.router.navigate(['/confirmation-success'], {
            queryParams: { message: 'Your account has been confirmed succesfully.' }
          });
        },
        error: (error) => {
          // Handle error
          console.log('Error:', error);
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

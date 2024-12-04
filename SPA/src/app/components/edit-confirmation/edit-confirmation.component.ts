import { Component, OnInit } from '@angular/core';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-edit-confirmation',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './edit-confirmation.component.html',
  styleUrl: './edit-confirmation.component.css'
})
export class EditConfirmationComponent implements OnInit {
  token = '';
  email = '';
  emailToEdit = '';
  phoneNumberToEdit = '';

  constructor(private userService: UserService, private route: ActivatedRoute, private router: Router) { }
  
  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.token = params['token'];
      this.email = params['email'];
      this.emailToEdit = params['emailToEdit'];
      this.phoneNumberToEdit = params['phoneNumberToEdit'];
      this.confirmEdit();
    });
  }

  confirmEdit() {
    if (this.token && this.email) {
      var confirmEditUser = {
        token: this.token,
        email: this.email,
        emailToEdit: this.emailToEdit,
        phoneNumberToEdit: this.phoneNumberToEdit,
      }
      this.userService.confirmEdit(confirmEditUser).subscribe({
        next: (response) => {
          // Handle success
          console.log('User updated:', response);
          this.router.navigate(['/confirmation-success'], {
            queryParams: { message: 'Your account has been updated succesfully.' }
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
        queryParams: { message: 'Invalid token or email.' }
      });
    }
  }

}

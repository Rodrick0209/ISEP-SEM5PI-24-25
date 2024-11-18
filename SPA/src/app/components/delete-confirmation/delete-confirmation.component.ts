import { Component } from '@angular/core';
import { UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-delete-confirmation',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './delete-confirmation.component.html',
  styleUrl: './delete-confirmation.component.css'
})
export class DeleteConfirmationComponent {
  token = '';
  email = '';

  constructor(private userService: UserService, private route: ActivatedRoute, private router: Router) { }
  
  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.token = params['token'];
      this.email = params['email'];
      this.confirmDelete();
    });
  }

  confirmDelete() {
    if (this.token && this.email) {
      this.userService.confirmDelete(this.token, this.email).subscribe({
        next: (response) => {
          // Handle success
          console.log('User updated:', response);
          this.router.navigate(['/confirmation-success'], {
            queryParams: { message: 'Your account and all associated data has been deleted succesfully.' }
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

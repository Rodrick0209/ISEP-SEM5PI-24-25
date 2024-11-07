import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { AuthService } from '../../services/auth.service';
import { ActivatedRoute, Router } from '@angular/router';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-user-profile',
  standalone: true,
  imports: [],
  templateUrl: './user-profile.component.html',
  styleUrl: './user-profile.component.css'
})
export class UserProfileComponent implements OnInit {
  user: User | undefined;
  email: string | null = null;
  errorMessage: string | null = null;
  successMessage: string | null = null;

  constructor(private userService: UserService, private authService: AuthService, private router: Router, private messageService: MessageService) { }
  
  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.email = this.authService.extractEmailFromToken();
    if (this.email) {
      this.userService.getUserByEmail(this.email).subscribe({
        next: (data: User) => {
          this.user = data;
        },
        error: (err: any) => {
          console.error('Failed to fetch user', err);
          this.errorMessage = 'An error occurred while fetching user: ' + err.error.message;
        }
      });
    } else {
      this.errorMessage = 'User not found';
    }
  }

  userSettings(): void {
    console.log('User settings clicked: ' + this.email);
    this.router.navigate(['/settings', this.email]);
  }
}

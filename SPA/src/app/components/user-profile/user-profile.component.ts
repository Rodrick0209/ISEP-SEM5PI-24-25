import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { AuthService } from '../../services/auth.service';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { MessageService } from '../../services/message.service';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-user-profile',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './user-profile.component.html',
  styleUrl: './user-profile.component.css'
})
export class UserProfileComponent implements OnInit {
  user: User | undefined;
  
  email: string | null = null;
  successMessage: string | null = null;
  errorMessage: string | null = null; 

  constructor(private userService: UserService, private route: ActivatedRoute, private messageService : MessageService, private router: Router) { }
  
  ngOnInit(): void {
    this.successMessage = this.messageService.getMessage();
    this.email = this.route.snapshot.paramMap.get('email');
    if (this.email) {
      this.getUserByEmail(this.email);
    } else {
      this.errorMessage = 'Invalid user';
    }
  }

  userSettings(): void {
    console.log('User settings clicked: ' + this.email);
    this.router.navigate(['/settings', this.email]);
  }

  getUserByEmail(email: string): void {
    this.userService.getUserByEmail(email).subscribe({
      next: (data: User) => this.user = data,
      error: (err: any) => {
        console.log('Failed to fetch user details', err);
        this.errorMessage = 'Failed to fetch user details';
      }
    });
  };
}

import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
  selector: 'app-user-settings',
  standalone: true,
  imports: [],
  templateUrl: './user-settings.component.html',
  styleUrl: './user-settings.component.css'
})
export class UserSettingsComponent implements OnInit {
  user: User | undefined;
  email: string | null = null;
  errorMessage = '';
  
  constructor(private userService: UserService, private router: Router, private route: ActivatedRoute) { }
  
  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email');
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

  editUser(): void {
    console.log('Edit user clicked: ' + this.email);
    this.router.navigate(['/edit', this.email]);
  }
}

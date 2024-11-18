import { Component, OnInit } from '@angular/core';
import { User, UserService } from '../../services/user.service';
import { ActivatedRoute, Router, RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-user-settings',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './user-settings.component.html',
  styleUrl: './user-settings.component.css'
})
export class UserSettingsComponent implements OnInit {
  user: User | undefined;
  email: string | null = null;
  
  constructor(private router: Router, private route: ActivatedRoute) { }
  
  ngOnInit(): void {
    this.email = this.route.snapshot.paramMap.get('email');
  }

  editUser(): void {
    console.log('Edit user clicked: ' + this.email);
    this.router.navigate(['/edit', this.email]);
  }

  deleteUser(): void {
    console.log('Delete user clicked: ' + this.email);
    this.router.navigate(['/delete', this.email]);
  }
}

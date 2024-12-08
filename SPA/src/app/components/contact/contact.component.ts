import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';

@Component({
  selector: 'app-contact',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './contact.component.html',
  styleUrls: ['./contact.component.css']
})
export class ContactComponent {
  socialMediaLinks = [
    { name: 'Facebook', url: 'https://www.facebook.com', icon: 'fab fa-facebook-f' },
    { name: 'X', url: 'https://www.x.com', icon: 'fab fa-x-twitter' },
    { name: 'Instagram', url: 'https://www.instagram.com', icon: 'fab fa-instagram' },
    { name: 'LinkedIn', url: 'https://www.linkedin.com', icon: 'fab fa-linkedin-in' },
    { name: 'YouTube', url: 'https://www.youtube.com', icon: 'fab fa-youtube' },
  ];
}
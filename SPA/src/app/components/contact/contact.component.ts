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
    { name: 'Example', url: 'https://www.facebook.com', icon: 'fab fa-facebook-f' },
    { name: '@example123', url: 'https://www.x.com', icon: 'fab fa-x-twitter' },
    { name: 'example.123', url: 'https://www.instagram.com', icon: 'fab fa-instagram' },
    { name: 'Example', url: 'https://www.linkedin.com', icon: 'fab fa-linkedin-in' },
    { name: 'Example', url: 'https://www.youtube.com', icon: 'fab fa-youtube' },
  ];
}
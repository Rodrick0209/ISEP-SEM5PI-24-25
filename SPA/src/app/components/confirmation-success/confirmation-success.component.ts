import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-confirmation-success',
  standalone: true,
  imports: [],
  templateUrl: './confirmation-success.component.html',
  styleUrl: './confirmation-success.component.css'
})
export class ConfirmationSuccessComponent implements OnInit{
  successMessage: string = '';

  constructor(private route: ActivatedRoute) { }

  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.successMessage = params['message'] || 'An error occurred during confirmation.';
    });
  }
}

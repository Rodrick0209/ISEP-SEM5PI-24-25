import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';

@Component({
  selector: 'app-confirmation-error',
  standalone: true,
  imports: [],
  templateUrl: './confirmation-error.component.html',
  styleUrl: './confirmation-error.component.css'
})
export class ConfirmationErrorComponent implements OnInit {
  errorMessage: string = '';

  constructor(private route: ActivatedRoute) { }

  ngOnInit(): void {
    this.route.queryParams.subscribe(params => {
      this.errorMessage = params['message'] || 'An error occurred during confirmation.';
    });
  }
}

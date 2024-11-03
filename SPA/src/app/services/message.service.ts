import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class MessageService {
  private message: string | null = null;

  setMessage(message: string): void {
    this.message = message;
  }

  getMessage(): string | null {
    const tempMessage = this.message;
    this.message = null; // Clear the message after retrieval
    return tempMessage;
  }
}

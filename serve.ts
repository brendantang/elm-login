import { serve } from "https://deno.land/std@0.114.0/http/server.ts";

async function handler(req: Request): Promise<Response> {
  // Use the request pathname as filepath
  const url = new URL(req.url);
  const filepath = decodeURIComponent(url.pathname);

  // Try opening the file
  let file;
  try {
    file = await Deno.readFile(`${Deno.cwd()}/public/` + filepath);
  } catch {
    // If the file cannot be opened, return a "404 Not Found" response
    return new Response("404 Not Found", { status: 404 });
  }

  // Build and send the response
  return new Response(file);
}

serve(handler);
